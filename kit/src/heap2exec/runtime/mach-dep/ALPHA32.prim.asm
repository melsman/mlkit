/* ALPHA32.prim.asm
 *
 * ALPHA32 runtime code for ML.
 *
 * ML register usage follows:
 *
 *    register  C callee	ML use
 *		  save?
 *    --------  ---------       -------
 *	$0         no		standard arg
 *	$1	   no		standard continuation
 *	$2	   no		standard closure
 *	$3	   no		standard link register
 *      $4	   no		base address register
 *	$5	   no		internal temporary (ptrtmp)
 *	$6-$8      no		miscellaneous registers
 *	$9	   yes		heap limit pointer
 *	$10	   yes		var pointer
 *	$11	   yes		heap-limit comparison flag, and arith temporary
 *	$12	   yes		store list pointer  (not used)
 *	$13	   yes		allocation pointer
 *	$14	   yes		exception continuation
 *	$15	   yes		instruction counter
 *	$16-$26    no		miscellaneous registers
 *	$27        no		gc link register
 *	$28        no		assembler temporary
 *	$29         -		reserved for C (global pointer)
 *	$30         -		reserved for C (stack pointer)
 */

#include <regdef.h>
#include "ml-base.h"
#include "asm-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-request.h"
#include "reg-mask.h"
#include "ml-limits.h"
#include "mlstate-offsets.h"	/** this file is generated **/


#define STDARG		$0 	/* standard arg  (ml_arg)	 		*/
#define STDCONT		$1	/* standard continuation (ml_cont) 	  	*/
#define STDCLOS		$2	/* standard closure (ml_closure)             	*/
#define STDLINK		$3	/* ptr to just-entered std function (ml_link)	*/
#define BASEPTR		$4	/* pointer to base of code object - 4 */
#define PTRTMP		$5	/* internal temporary 				*/
#define MISCREG0	$6
#define MISCREG1	$7
#define MISCREG2	$8
#define LIMITPTR	$9	/* end of heap - 4096  (ml_limitptr)  	  	*/
#define VARPTR		$10 	/* per-thread var pointer (ml_varptr)		*/
#define NEEDGC		$11 	/* arith temp; also, heap-limit comparison flag	*/
#define STOREPTR	$12 	/* store pointer  (ml_storeptr) 		*/
#define ALLOCPTR	$13 	/* freespace pointer  (ml_allocptr) 		*/
#define EXNCONT		$14 	/* exception handler (ml_exncont) 		*/
/* #define icountr		$15 */
#define MISCREG3	$16
#define MISCREG4	$17
#define MISCREG5	$18
#define MISCREG6	$19
#define MISCREG7	$20
#define MISCREG8	$21
#define MISCREG9	$22
#define MISCREG10	$23
#define MISCREG11	$24
#define MISCREG12	$25
#define GCLINK		$26	/* resumption point for restoreregs (ml_pc)	*/
#define MISCREG13	$27
/* assembler-temp $28 						 	*/
/*      globalptr $29	   reserved for C and assembler			*/
/*       stackptr $30        stack pointer 				*/
/*           zero $31        zero 					*/

#define CRESULT		$0
#define CARG0		$16

#define ATMP1		$20
#define ATMP2		$21
#define ATMP3		$22
#define ATMP4		$23

/* The root registers in the ML state vector have the following layout,
 * where roots is guaranteed to be 8-byte aligned relative to the start
 * of the ML state vector (see "ml-state.h"):
 *
 ******** THIS IS OUT OF DATE *******
 *
 *			+-------------------+
 *	roots:   	|    ml_arg ($0)    |
 *			+-------------------+
 *	roots+4: 	|    ml_cont ($1)   |
 *			+-------------------+
 *	roots+8: 	|  ml_closure ($2)  |
 *			+-------------------+
 *	roots+12:	|  ml_linkReg ($3)  |
 *			+-------------------+
 *	roots+16:	|    ml_pc  ($27)   |
 *			+-------------------+
 *	roots+20:	|  icount ($15)     |
 *			+-------------------+
 *	roots+24:	| ($6,$7,$8,$16-26) |
 *			+-------------------+
 *	roots+80:	|  ml_varReg ($10)  |
 *			+-------------------+
 *      roots+84:	| ml_exncont ($14)  |
 *			+-------------------+
 *	roots+88:	|  ml_baseReg ($4)  |
 *			+-------------------+
 */


/* The  ML stack frame has the following layout (set up by restoreregs):
 *			+-------------------+
 *	sp+124		|    ml_divlu       |	
 *			+-------------------+ 
 *      sp+120          |    ml_divl        |
 *			+-------------------+
 *	sp+116		|   pseudo reg 2    |
 *			+-------------------+
 *	sp+112		|   pseudo reg 1    |
 *			+-------------------+
 *      sp+104:		| temporary storage |  temporary use by floating
 *			+-------------------+  point code.
 *      sp+96:		| temporary storage |
 *			+-------------------+
 *      sp+88: 		|     saved $30     |
 *			+-------------------+
 *      sp+80:		|     saved $29     |
 *			+-------------------+
 *      sp+72:		|     saved $26     |  is this needed??? - Ken Cline
 *			+-------------------+
 *      sp+64:		|     saved $15     |
 *			+-------------------+
 *      sp+56:		|     saved $14     |
 *			+-------------------+
 *      sp+48:		|     saved $13     |
 *			+-------------------+
 *      sp+40:		|     saved $12     |
 *			+-------------------+
 *      sp+32:		|     saved $11     |
 *			+-------------------+
 *      sp+24:		|     saved $10     |
 *			+-------------------+
 *      sp+16:		|     saved $9      |
 *			+-------------------+
 *	sp+8:		| addr of saveregs  |
 *			+-------------------+
 *      sp:		|  ptr to MLState   |
 *			+-------------------+
 */

#define ML_FRAMESIZE	4096
#define MLSTATE_OFFSET	0
#define STARTGC_OFFSET	8
#define REGSAVE_OFFSET	16
#define PSEUDOREG_OFFSET 112
#define	ML_DIVL_OFFSET 120
#define	ML_DIVLU_OFFSET	124

#ifdef ALLIGN_ALLOCATION
#  define ALLOCALIGN						\
	    addl	ALLOCPTR,4,ALLOCPTR;			\
	    bic		ALLOCPTR,4,ALLOCPTR;
#else
#  define ALLOCALIGN
#endif

#if (CALLEESAVE > 0)
#define CONTINUE						\
	    ALLOCALIGN						\
	    cmplt	LIMITPTR,ALLOCPTR,NEEDGC;		\
            jmp		(STDCONT);
#else
#define CONTINUE						\
	    ldl		STDLINK,0(STDCONT);			\
	    ALLOCALIGN						\
	    cmplt	LIMITPTR,ALLOCPTR,NEEDGC;		\
	    jmp		(STDLINK)
#endif

#define CHECKLIMIT(mask)					\
	    mov		mask,PTRTMP;				\
	    beq		NEEDGC,3f;				\
	    mov 	STDLINK,GCLINK;			\
	    br		saveregs;				\
	 3:


	DATA
	.align 3
one_half:	.t_floating	0.5
fsr_bits:	.quad		0x8a70000000000000
				
	TEXT


/* sigh_return_a:
 * The return continuation for the ML signal handler.
 */
ML_CODE_HDR(sigh_return_a)
	mov	RET_MASK,PTRTMP
	mov	REQ_SIG_RETURN,ATMP1
	br	set_request


/* sigh_resume:
 * Resume execution at the point at which a handler trap occurred.  This is a
 * standard two-argument function, thus the closure is in ml_cont (%stdcont).
 */
ENTRY(sigh_resume)
	mov	RET_MASK,PTRTMP
	mov	REQ_SIG_RESUME,ATMP1
	br	set_request

/* pollh_return_a
 * The return continuation for the ML poll handler.
 */
ML_CODE_HDR(pollh_return_a)
	mov	RET_MASK,PTRTMP
	mov	REQ_POLL_RESUME,ATMP1
	br	set_request

/* pollh_resume:
 * Resume execution at the point at which a poll event occurred.
 */
ENTRY(pollh_resume)
	mov	RET_MASK,PTRTMP
	mov	REQ_POLL_RETURN,ATMP1
	br	set_request

ML_CODE_HDR(handle_a)
	mov	EXN_MASK,PTRTMP
	mov	REQ_EXN,ATMP1
	br	set_request

ML_CODE_HDR(return_a)
	mov	RET_MASK,PTRTMP
	mov	REQ_RETURN,ATMP1
	br	set_request

ENTRY(request_fault)
	mov	EXN_MASK,PTRTMP
	mov	REQ_FAULT,ATMP1
	br	set_request

/* bind_cfun : (string * string) -> c_function
 */
ML_CODE_HDR(bind_cfun_a)
	CHECKLIMIT(FUN_MASK)
	mov	FUN_MASK,PTRTMP
	mov	REQ_BIND_CFUN,ATMP1
	br	set_request

ML_CODE_HDR(build_literals_a)
	CHECKLIMIT(FUN_MASK)
	mov	FUN_MASK,PTRTMP
	mov	REQ_BUILD_LITERALS,ATMP1
	br	set_request

ML_CODE_HDR(callc_a)
	CHECKLIMIT(FUN_MASK)
	mov	FUN_MASK,PTRTMP
	mov	REQ_CALLC,ATMP1
	/* fall through */

set_request:			/* a quick return to run_ml(), ptrtmp holds */
				/* the request code, and atmp1 holds the  */
				/* live register mask. */

	mov	PTRTMP,NEEDGC			/* save the register mask */
	ldq	PTRTMP,MLSTATE_OFFSET(sp)	/* get the ML state ptr from the stack */
	stl	NEEDGC,MaskOffMSP(PTRTMP)
	ldq	NEEDGC,VProcOffMSP(PTRTMP)	/* use NEEDGC for VProcPtr */
	stl	zero,InMLOffVSP(NEEDGC)		/* note that we have left ML */
	stl	ALLOCPTR,AllocPtrOffMSP(PTRTMP)
	stl	LIMITPTR,LimitPtrOffMSP(PTRTMP)
	stl	STOREPTR,StorePtrOffMSP(PTRTMP)
	stl	STDLINK,LinkRegOffMSP(PTRTMP)
	stl	STDLINK,PCOffMSP(PTRTMP)	/* address of called function */
	stl	STDARG,StdArgOffMSP(PTRTMP)
	stl	STDCLOS,StdClosOffMSP(PTRTMP)
	stl	STDCONT,StdContOffMSP(PTRTMP)
	stl	VARPTR,VarPtrOffMSP(PTRTMP)
	stl	EXNCONT,ExnPtrOffMSP(PTRTMP)
	mov	ATMP1,CRESULT				/* return request */
#if (CALLEESAVE > 0)
	stl	MISCREG0,MiscRegOffMSP(0)(PTRTMP)
#if (CALLEESAVE > 1)
	stl	MISCREG1,MiscRegOffMSP(1)(PTRTMP)
#if (CALLEESAVE > 2)
	stl	MISCREG2,MiscRegOffMSP(2)(PTRTMP)
#if (CALLEESAVE > 3)
	stl	MISCREG3,MiscRegOffMSP(3)(PTRTMP)
#if (CALLEESAVE > 4)
	stl	MISCREG4,MiscRegOffMSP(4)(PTRTMP)
#if (CALLEESAVE > 5)
	stl	MISCREG5,MiscRegOffMSP(5)(PTRTMP)
#if (CALLEESAVE > 6)
	stl	MISCREG6,MiscRegOffMSP(6)(PTRTMP)
#if (CALLEESAVE > 7)
	stl	MISCREG7,MiscRegOffMSP(7)(PTRTMP)
#if (CALLEESAVE > 8)
	stl	MISCREG8,MiscRegOffMSP(8)(PTRTMP)
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
					/* restore callee-save C registers */
restore_c_regs:
	ldl     ATMP1,PSEUDOREG_OFFSET(sp)
	stl     ATMP1,PseudoReg1OffMSP(PTRTMP)
 	ldl	ATMP1,PSEUDOREG_OFFSET+4(sp)
	stl     ATMP1,PseudoReg2OffMSP(PTRTMP)
	ldq	$30,REGSAVE_OFFSET+72(sp)
	ldq	$29,REGSAVE_OFFSET+64(sp)
	ldq	$26,REGSAVE_OFFSET+56(sp)
        ldq     $15,REGSAVE_OFFSET+48(sp)
        ldq     $14,REGSAVE_OFFSET+40(sp)
        ldq     $13,REGSAVE_OFFSET+32(sp)
        ldq     $12,REGSAVE_OFFSET+24(sp)
        ldq     $11,REGSAVE_OFFSET+16(sp)
        ldq     $10,REGSAVE_OFFSET+8(sp)
        ldq     $9 ,REGSAVE_OFFSET(sp)
	addq	sp,ML_FRAMESIZE				/* discard the stack frame */
	jmp	($26)				/* return to run_ml() */

	BEGIN_PROC(saveregs)
ENTRY(saveregs)
	mov	PTRTMP,NEEDGC		    /* save the register mask */
	ldq	PTRTMP,MLSTATE_OFFSET(sp)   /* use ptrtmp to access ML state */
	stl	NEEDGC,MaskOffMSP(PTRTMP)
1:
	ldq	NEEDGC,VProcOffMSP(PTRTMP)  /* use NEEDGC for VProcPtr */
	stl	zero,InMLOffVSP(NEEDGC)	    /* note that we have left ML */
	subl    BASEPTR,32764  		    /* adjust baseReg */
	stl	ALLOCPTR,AllocPtrOffMSP(PTRTMP)
	stl	LIMITPTR,LimitPtrOffMSP(PTRTMP)
	stl	STOREPTR,StorePtrOffMSP(PTRTMP)
	stl	STDARG,StdArgOffMSP(PTRTMP)
	stl	STDCONT,StdContOffMSP(PTRTMP)
	stl	STDCLOS,StdClosOffMSP(PTRTMP)
	stl	GCLINK,PCOffMSP(PTRTMP)
	stl	EXNCONT,ExnPtrOffMSP(PTRTMP)
	stl	MISCREG0,MiscRegOffMSP(0)(PTRTMP)	/* save misc. roots */
	stl	MISCREG1,MiscRegOffMSP(1)(PTRTMP)
	stl	MISCREG2,MiscRegOffMSP(2)(PTRTMP)
	stl	MISCREG3,MiscRegOffMSP(3)(PTRTMP)
	stl	MISCREG4,MiscRegOffMSP(4)(PTRTMP)
	stl	MISCREG5,MiscRegOffMSP(5)(PTRTMP)
	stl	MISCREG6,MiscRegOffMSP(6)(PTRTMP)
	stl	MISCREG7,MiscRegOffMSP(7)(PTRTMP)
	stl	MISCREG8,MiscRegOffMSP(8)(PTRTMP)
	stl	MISCREG9,MiscRegOffMSP(9)(PTRTMP)
	stl	MISCREG10,MiscRegOffMSP(10)(PTRTMP)
	stl	MISCREG11,MiscRegOffMSP(11)(PTRTMP)
	stl	MISCREG12,MiscRegOffMSP(12)(PTRTMP)
	stl	MISCREG13,MiscRegOffMSP(13)(PTRTMP)
	stl	STDLINK,LinkRegOffMSP(PTRTMP)
	stl	BASEPTR,BasePtrOffMSP(PTRTMP)		/* base reg */
	stl	VARPTR,VarPtrOffMSP(PTRTMP)
	mov	REQ_GC,CRESULT
	br	restore_c_regs
	.end	saveregs


	BEGIN_PROC(restoreregs)
ENTRY(restoreregs)
	subq	sp,ML_FRAMESIZE			/* allocate a stack frame */
	.frame	sp,ML_FRAMESIZE,zero
	.mask	0xe000fe00,0
						/* save the C registers */
	lda	$3,saveregs
	stq	CARG0,MLSTATE_OFFSET(sp)	/* save MLState ptr for return to C */
	stq	$3,STARTGC_OFFSET(sp)		/* so ML can find saveregs! */
	lda     $3,ml_divl			/* address of ml_divl */
	stl	$3,ML_DIVL_OFFSET(sp)
	lda	$3,ml_divlu			/* address of ml_divlu */
	stl	$3,ML_DIVLU_OFFSET(sp)
	stq	$30,REGSAVE_OFFSET+72(sp)
        stq     $29,REGSAVE_OFFSET+64(sp)
        stq     $26,REGSAVE_OFFSET+56(sp)
        stq     $15,REGSAVE_OFFSET+48(sp)
        stq     $14,REGSAVE_OFFSET+40(sp)
        stq     $13,REGSAVE_OFFSET+32(sp)
        stq     $12,REGSAVE_OFFSET+24(sp)
        stq     $11,REGSAVE_OFFSET+16(sp)
        stq     $10,REGSAVE_OFFSET+8(sp)
        stq     $9,REGSAVE_OFFSET(sp)
	mov     CARG0,PTRTMP			/* put MLState ptr in ptrtmp */

	ldl 	ATMP1,PseudoReg1OffMSP(PTRTMP)
	stl 	ATMP1,PSEUDOREG_OFFSET(sp)
	ldl 	ATMP1,PseudoReg2OffMSP(PTRTMP)
	stl 	ATMP1,PSEUDOREG_OFFSET+4(sp)

	ldl	ALLOCPTR,AllocPtrOffMSP(PTRTMP)
	ldl	LIMITPTR,LimitPtrOffMSP(PTRTMP)
	ldl	STOREPTR,StorePtrOffMSP(PTRTMP)
	ldl	NEEDGC,VProcOffMSP(PTRTMP)	/* use NEEDGC for VProc Ptr */
	mov	1,ATMP1
.set	noreorder			/* the order here is important */
	stl	ATMP1,InMLOffVSP(NEEDGC)	/* note that we are entering ML code */
	ldl	STDARG,StdArgOffMSP(PTRTMP)
	ldl	STDCONT,StdContOffMSP(PTRTMP)
	ldl	STDCLOS,StdClosOffMSP(PTRTMP)
	ldl	EXNCONT,ExnPtrOffMSP(PTRTMP)
	ldl	MISCREG0,MiscRegOffMSP(0)(PTRTMP)
	ldl	MISCREG1,MiscRegOffMSP(1)(PTRTMP)
	ldl	MISCREG2,MiscRegOffMSP(2)(PTRTMP)
	ldl	MISCREG3,MiscRegOffMSP(3)(PTRTMP)
	ldl	MISCREG4,MiscRegOffMSP(4)(PTRTMP)
	ldl	MISCREG5,MiscRegOffMSP(5)(PTRTMP)
	ldl	MISCREG6,MiscRegOffMSP(6)(PTRTMP)
	ldl	MISCREG7,MiscRegOffMSP(7)(PTRTMP)
	ldl	MISCREG8,MiscRegOffMSP(8)(PTRTMP)
	ldl	MISCREG9,MiscRegOffMSP(9)(PTRTMP)
	ldl	MISCREG10,MiscRegOffMSP(10)(PTRTMP)
	ldl	MISCREG11,MiscRegOffMSP(11)(PTRTMP)
	ldl	MISCREG12,MiscRegOffMSP(12)(PTRTMP)
	ldl	MISCREG13,MiscRegOffMSP(13)(PTRTMP)
	ldl	STDLINK,LinkRegOffMSP(PTRTMP)
	ldl	VARPTR,VarPtrOffMSP(PTRTMP)
	ldl 	BASEPTR,BasePtrOffMSP(PTRTMP)
	addl    BASEPTR,32764			/* adjust baseReg */
	ldl	GCLINK,PCOffMSP(PTRTMP)
						/* check for pending signals */
	ldl	PTRTMP,NPendingSysOffVSP(NEEDGC)
.set	noat
	ldl	$28,NPendingOffVSP(NEEDGC)
	addq	PTRTMP,$28,PTRTMP
.set	at
	bne	PTRTMP,pending_sigs 
	.end	restoreregs
	.ent	ml_go
ENTRY(ml_go)
	ALLOCALIGN
	cmplt	LIMITPTR,ALLOCPTR,NEEDGC
	jmp	(GCLINK)			/* jump to ML code */
	.end	ml_go

pending_sigs:	/* there are pending signals */
					/* check if we are currently handling a signal */
	ldl	PTRTMP,InSigHandlerOffVSP(NEEDGC)
	bne	PTRTMP,ml_go	
					/* note that a handler trap is pending */
	mov	1,PTRTMP
	stl	PTRTMP,HandlerPendingOffVSP(NEEDGC)
	mov	ALLOCPTR,LIMITPTR
	br	ml_go
.set	reorder


/* SaveFPRegs:
 *
 *   void SaveFPRegs (Word_t *p)
 *
 * Save the C callee-save FP registers starting at the given address.
 */
	TEXT
	BEGIN_PROC(SaveFPRegs)
ENTRY(SaveFPRegs)
	stt	$f2,0(a0)
	stt	$f3,8(a0)
	stt	$f4,16(a0)
	stt	$f5,24(a0)
	stt	$f6,32(a0)
	stt	$f7,40(a0)
	stt	$f8,48(a0)
	stt	$f9,56(a0)
	jmp 	(ra)			/* return */
	END_PROC(SaveFPRegs)

/* RestoreFPRegs:
 *
 *   void RestoreFPRegs (Word_t *p)
 *
 * Restore the C callee-save FP registers from the given address.
 */
	BEGIN_PROC(RestoreFPRegs)
ENTRY(RestoreFPRegs)			/* floats address passed as parm */
	ldt	$f2,0(a0)		/* retrieve float registers */
	ldt	$f3,8(a0)
	ldt	$f4,16(a0)
	ldt	$f5,24(a0)
	ldt	$f6,32(a0)
	ldt	$f7,40(a0)
	ldt	$f8,48(a0)
	ldt	$f9,56(a0)
	jmp	(ra)
	END_PROC(RestoreFPRegs)


/** Primitive object allocation routines **/

/* array : (int * 'a) -> 'a array
 * Allocate and initialize a new array.	 This can cause GC.
 */
ML_CODE_HDR(array_a)
	CHECKLIMIT(FUN_MASK)
	ldl	ATMP1,0(STDARG)	    /* atmp1 := length (tagged int) */
	sra	ATMP1,1		    /* atmp1 := length (untagged int) */
	subq	ATMP1,SMALL_OBJ_SZW,ATMP3
	bgt	ATMP3,1f		    /* is this a small object? */
	sll	ATMP1,TAG_SHIFTW,ATMP3    /* build descriptor in atmp3 */
	or	ATMP3,MAKE_TAG(DTAG_array),ATMP3
	stl	ATMP3,0(ALLOCPTR)	    /* store descriptor */
	addq	ALLOCPTR,4		    /* allocptr++ */
	ldl	ATMP2,4(STDARG)	    /* atmp2 := initial value */
	mov	ALLOCPTR,STDARG
	sll	ATMP1,2,ATMP1		    /* atmp1 := length in bytes */
	addq	ATMP1,ALLOCPTR	    /* atmp1 is end of array */
2:					    /* loop: */
	stl	ATMP2,0(ALLOCPTR)		/* store next element */
	addq	ALLOCPTR,4			/* allocptr++ */
	subq	ALLOCPTR,ATMP1,ATMP4
	bne	ATMP4,2b			/* if (allocptr != end) goto loop */
					    /* end loop */
	CONTINUE

1:	/* off-line allocation of big arrays */
	mov	FUN_MASK,PTRTMP
	mov	REQ_ALLOC_ARRAY,ATMP1
	br	set_request

/* create_r : int -> realarray
 * Create a new realarray.
 */
ML_CODE_HDR(create_r_a)
	CHECKLIMIT(FUN_MASK)
	sra	STDARG,1,ATMP1	    /* atmp1 = length (untagged int) */
	sll	ATMP1,1,ATMP2		    /* atmp2 = length in words */
	subl	ATMP2,SMALL_OBJ_SZW,ATMP3
	bgt	ATMP3,1f		    /* is this a small object? */
	sll	ATMP1,TAG_SHIFTW,ATMP3    /* build descriptor in atmp3 */
	or	ATMP3,MAKE_TAG(DTAG_realdarray),ATMP3
#ifdef ALIGN_REALDS
	or	ALLOCPTR,4,ALLOCPTR	    /* tag is unaligned, so that the */
					    /* first element is 8-byte aligned */
#endif
	stl	ATMP3,0(ALLOCPTR)
	addq	ALLOCPTR,4,STDARG	    /* pointer to new realarray */
	sll	ATMP2,2		    /* atmp2 = length in bytes (no tag) */
	addq	ATMP2,4		    /* plus tag */
	addq	ALLOCPTR,ATMP2	    /* allocptr += total length */
	CONTINUE

1:	/* off-line allocation of big realarrays */
	mov	FUN_MASK,PTRTMP
	mov	REQ_ALLOC_REALDARRAY,ATMP1
	br	set_request

/* create_b : int -> bytearray
 * Create a bytearray of the given length.
 */
ML_CODE_HDR(create_b_a)
	CHECKLIMIT(FUN_MASK)
	sra	STDARG,1,ATMP1	    /* atmp1 = length (untagged int) */
	addq	ATMP1,3,ATMP2		    /* atmp2 = length in words */
	sra	ATMP2,2
	subq	ATMP2,SMALL_OBJ_SZW,ATMP3 /* is this a small object? */
	bgt	ATMP3,1f
	sll	ATMP1,TAG_SHIFTW,ATMP3    /* build descriptor in atmp3 */
	or	ATMP3,MAKE_TAG(DTAG_bytearray),ATMP3
	stl	ATMP3,0(ALLOCPTR)
	addq	ALLOCPTR,4,STDARG	    /* pointer to new bytearray */
	sll	ATMP2,2		    /* atmp2 = length in bytes (no tag) */
	addq	ATMP2,4		    /* plus tag */
	addq	ALLOCPTR,ATMP2,ALLOCPTR  /* allocptr += total length */
	CONTINUE
1:					/* off-line allocation of big bytearrays */
	mov	FUN_MASK,PTRTMP
	mov	REQ_ALLOC_BYTEARRAY,ATMP1
	br	set_request

/* create_s : int -> string
 * Create a string of the given length (assume length >0).
 */
ML_CODE_HDR(create_s_a)
	CHECKLIMIT(FUN_MASK)
	sra	STDARG,1,ATMP1	    /* atmp1 = length (untagged int) */
	addq	ATMP1,4,ATMP2		    /* atmp2 = length in words */
	sra	ATMP2,2
	subq	ATMP2,SMALL_OBJ_SZW,ATMP3
	bgt	ATMP3,1f		    /* is this a small object? */
	sll	ATMP1,TAG_SHIFTW,ATMP3    /* build descriptor in atmp3 */
	or	ATMP3,MAKE_TAG(DTAG_string),ATMP3
	stl	ATMP3,0(ALLOCPTR)
	addq	ALLOCPTR,4,STDARG	    /* pointer to new string */
	sll	ATMP2,2		    /* atmp2 = length in bytes (no tag) */
	addq	ATMP2,4		    /* plus tag */
	addq	ALLOCPTR,ATMP2	    /* allocptr += total length */
	stl	zero,-4(ALLOCPTR)	    /* store zero in last word */
	CONTINUE
1:					/* off-line allocation of big strings */
	mov	FUN_MASK,PTRTMP
	mov	REQ_ALLOC_STRING,ATMP1
	br	set_request

/* create_v_a : (int * 'a list) -> 'a vector
 * Create a vector with elements taken from a list.
 * NOTE: the front-end ensures that list cannot be nil.
 */
ML_CODE_HDR(create_v_a)
	CHECKLIMIT(FUN_MASK)
	ldl	ATMP1,0(STDARG)	    /* atmp1 := length (tagged int) */
	sra	ATMP1,1		    /* atmp1 := length (untagged) */
	subq	ATMP1,SMALL_OBJ_SZW,ATMP2
	bgt	ATMP2,1f	    /* is this a small object? */
	sll	ATMP1,TAG_SHIFTW,ATMP2    /* build descriptor in atmp2 */
	or	ATMP2,MAKE_TAG(DTAG_vector),ATMP2
	stl	ATMP2,0(ALLOCPTR)	    /* store descriptor */
	addq	ALLOCPTR,4		    /* allocptr++ */
	ldl	ATMP2,4(STDARG)	    /* atmp2 := list */
	mov	ALLOCPTR,STDARG	    /* stdarg := vector */
	mov	ML_nil,ATMP3
2:					    /* loop: */
	ldl	ATMP1,0(ATMP2)	        /* atmp1 := hd(atmp2) */
	ldl	ATMP2,4(ATMP2)	        /* atmp2 := tl(atmp2) */
	stl	ATMP1,0(ALLOCPTR)	        /* store word in vector */
	addq	ALLOCPTR,4		        /* allocptr++ */
	subq	ATMP2,ATMP3,ATMP4
	bne	ATMP4,2b			/* if (atmp2 <> nil) goto loop */
					    /* end loop */
	CONTINUE

1:	/* off-line allocation for large vectors */
	mov	FUN_MASK,PTRTMP
	mov	REQ_ALLOC_VECTOR,ATMP1
	br	set_request

/* Floating exceptions raised (assuming ROP's are never passed to functions):
 *	DIVIDE BY ZERO - (div)
 *	OVERFLOW/UNDERFLOW - (add,div,sub,mul) as appropriate
 *
 * floor raises integer overflow if the float is out of 32-bit range,
 * Does not check for out-of-range ;  it's up to the ML code to do that first. */
#ifdef NEW_FLOOR
		DATA
		.align 3
floor_MAXFLOAT: .quad 0x4330080000000000

		.text

ML_CODE_HDR(floor_a)
					/* check for overflow */
	ldgp	gp, 0(STDLINK)
	ldt	$f0, 0(STDARG)

	subq	$30, 16, $30		/* allocate stack space */
				/* Do floor; neat thing is that this works
				** for both +ve and -ve floating point numbers!
			        */
	ldt	$f1, floor_MAXFLOAT
	addtm	$f0, $f1, $f1
	stt	$f1, 0($30)
	ldl	ATMP1, 0($30)
	addl    ATMP1, ATMP1, ATMP1
	addl	ATMP1, 1, STDARG

	addq	$30, 16, $30 	
	CONTINUE

#else /* !NEW_FLOOR */
		DATA
		.align 3
floor_HALF:	.t_floating  0.5

		.text

ML_CODE_HDR(floor_a)
	ldgp	gp, 0(STDLINK)
	ldt	$f0, 0(STDARG)		/* get argument */
	subq	$30, 16, $30		/* allocate stack space */
	fblt	$f0, floor_negative_arg

floor_positive_arg:
	cvttqc	$f0, $f1
	stt	$f1, 0($30)
	ldl	ATMP1, 0($30)
	addl    ATMP1, ATMP1, ATMP1
	addl	ATMP1, 1, STDARG

	addq	$30, 16, $30
	CONTINUE


floor_negative_arg:

	/* cvttqm (x) = cvttq (2*x - 0.5) / 2 */
	/* cvttq (x-0.5) loses for odd integers which IEEE round to evens */
	ldt	$f1, floor_HALF
	addt	$f0, $f0, $f0
	subt	$f0, $f1, $f0
	cvttq	$f0, $f0
	stt	$f0, 0($30)
	ldl	STDARG, 0($30)
	/* STDARG now holds either 2*floor(x) or 2*floor(x)+1.  */
	/* convert to ml int by setting least bit! */
	bis	STDARG, 1, STDARG

	addq	$30, 16, $30
	CONTINUE

#endif
	

ML_CODE_HDR(logb_a)
	ldq 	STDARG,(STDARG)	/* get argument */
	srl 	STDARG,52		/* throw out 52 low bits */
	and	STDARG,0x07ff		/* clear all but 11 low bits */
	subq 	STDARG,1023		/* subtract 1023 */
	sll 	STDARG,1		/* make room for tag bit */
	addq	STDARG,1		/* add the tag bit */
	CONTINUE

ML_CODE_HDR(scalb_a)
	CHECKLIMIT(FUN_MASK)
	ldl	PTRTMP,0(STDARG)	/* address of float */
	ldq	ATMP2,0(PTRTMP)	/* get float */
	ldl	ATMP1,4(STDARG)	/* get tagged n */
	sra	ATMP1,1,ATMP1		/* real n */
	beq	ATMP1,9f		/* branch if n=0 */
	sra	ATMP2,52,ATMP3	/* shift out fraction of float */
	and	ATMP3,0xfff,ATMP3	/* just exponent of float */
	addq	ATMP3,ATMP1,ATMP3	/* n + exponent */
	ble	ATMP3,6f		/* branch if underflow */
	sll	ATMP1,52,ATMP1	/* n in exponent position */
	addqv	ATMP2,ATMP1,ATMP1	/* add n to exponent, with overflow */
3:	/* return float in atmp1 */
	or	ALLOCPTR,4,ALLOCPTR	/* unalign allocptr to align float */
	mov	DESC_reald,ATMP2	/* get desc */
	stl	ATMP2,0(ALLOCPTR)	/* store desc */
	stq	ATMP1,4(ALLOCPTR)	/* store float */
	addq	ALLOCPTR,4,STDARG	/* return boxed float */
	addq	ALLOCPTR,12,ALLOCPTR	/* set allocptr */
	CONTINUE
6:	/* underflow -- return zero */
	mov	0,ATMP1
	br	3b
9:	/* n=0  --  return original float */
	mov	PTRTMP,STDARG
	CONTINUE

/* ml_divl
 * Incoming parameters in $16 and $17, result in $0	
 */
ENTRY(ml_divl)			/* divide longword */
	beq $17, divZero        /* check for div-by-zero */
	ornot $31, $17, $0	/* is divisor -1 */
	bne $0, do_ml_divl	/* NO */
	sublv $31, $16, $0	/* is dividend largest negative int */
	trapb			/* YES */
do_ml_divl:	
	divl $16, $17, $0	/* do divl */
	ret $31, ($26), 1


/* ml_divlu
 * Incoming parameters in $16 and $17, result in $0	
 */
ENTRY(ml_divlu)			/* divide longwork unsigned */ 
	beq $17, divZero	/* check for div-by-zero */
	divlu $16, $17, $0	/* do divlu */
	ret $31, ($26), 1

divZero:
	lda $16, -2($31)	/* generate div-by-zero */
	call_pal 0xaa		/* gentrap */

		
/* try_lock : spin_lock -> bool
 * low-level test-and-set style primitive for mutual-exclusion among 
 * processors.
 */
ML_CODE_HDR(try_lock_a)
#if (MAX_PROCS > 1)
	???
#else (MAX_PROCS == 1)
	ldl	ATMP1,0(STDARG)
	mov	ML_false,ATMP2
	stl	ATMP2,0(STDARG)
	mov	ATMP1,STDARG
	CONTINUE
#endif

/* unlock : releases a spin lock 
 */
ML_CODE_HDR(unlock_a)
#if (MAX_PROCS > 1)
	???
#else (MAX_PROCS == 1)
	mov	ML_true,ATMP1
	stl	ATMP1,0(STDARG)
	mov	ML_unit,STDARG
	CONTINUE
#endif

/* SetFSR:
 * Turn on floating-point overflow, underflow and zero-divide exceptions.
 */
	BEGIN_PROC(SetFSR)
ENTRY(SetFSR)
	trapb				/* trap barrier just in case */
	ldt	$f1,fsr_bits		/* normal rounding,iov,ovf,dze,inv */
	mt_fpcr	$f1			/* set floating point control reg */
	jmp	($26)
	END_PROC(SetFSR)

/* FlushICache:
 * C callable instruction cache flush function
 */
	BEGIN_PROC(FlushICache)
ENTRY(FlushICache)
	.frame	$30,0,$26,0
	.prologue 0
	call_pal 0x86			/* imb */
	ret	$31,($26),1
	END_PROC(FlushICache)

