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
 *	$5-$8      no		miscellaneous registers
 *	$9	   yes		heap limit pointer
 *	$10	   yes		var pointer
 *	$11	   yes		heap-limit comparison flag, and arith temporary
 *	$12	   yes		store list pointer
 *	$13	   yes		allocation pointer
 *	$14	   yes		exception continuation
 *	$15	   yes		miscellaneous register
 *	$16-$25    no		miscellaneous registers
 *	$26	   no		ml-pc
 *	$27        no		miscellaneous register
 *	$28        no		assembler temporary
 *	$29         -		reserved for C (global pointer)
 *	$30         -		reserved for C (stack pointer)
 *	$31	    -           constant zero
 */

#include <regdef.h>
#include "ml-base.h"
#include "asm-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-request.h"
#include "ml-limits.h"
#include "mlstate-offsets.h"	/** this file is generated **/


#define STDARG		$0 	/* standard arg  (ml_arg)	 		*/
#define STDCONT		$1	/* standard continuation (ml_cont) 	  	*/
#define STDCLOS		$2	/* standard closure (ml_closure)             	*/
#define STDLINK		$3	/* ptr to just-entered std function (ml_link)	*/
#define MISCREG0	$5
#define MISCREG1	$6
#define MISCREG2	$7
#define LIMITPTR	$9	/* end of heap - 4096  (ml_limitptr)  	  	*/
#define VARPTR		$10 	/* per-thread var pointer (ml_varptr)		*/
#define NEEDGC		$11 	/* arith temp; also, heap-limit comparison flag	*/
#define STOREPTR	$12 	/* store pointer  (ml_storeptr) 		*/
#define ALLOCPTR	$13 	/* freespace pointer  (ml_allocptr) 		*/
#define EXNCONT		$14 	/* exception handler (ml_exncont) 		*/
#define PC		$26	/* address to return/goto in ML			*/
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
#define PTRTMP		$24	

/* The  ML stack frame has the following layout (set up by restoreregs):
 *			+-------------------+
 *	sp+124		|    ml_divlu       |	
 *			+-------------------+ 
 *      sp+120          |    ml_divlv       |
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
 *      sp+72:		|     saved $26     | 
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
#define	ML_DIVLV_OFFSET 120
#define	ML_DIVLU_OFFSET	124

#ifdef ALLIGN_ALLOCATION
#  define ALLOCALIGN						\
	    addl	ALLOCPTR,4,ALLOCPTR;			\
	    bic		ALLOCPTR,4,ALLOCPTR;
#else
#  define ALLOCALIGN
#endif

#define CONTINUE					\
	    ALLOCALIGN					\
	    cmplt	LIMITPTR,ALLOCPTR,NEEDGC;	\
            jmp		(STDCONT);

#define CHECKLIMIT					\
	    mov		STDLINK, PC;			\
	    beq		NEEDGC,3f;			\
	    br		saveregs;			\
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
	mov	REQ_SIG_RETURN,ATMP1
	mov	ML_unit, STDLINK
	mov	ML_unit, STDCLOS
	mov	ML_unit, PC
	br	set_request


/* sigh_resume:
 * Resume execution at the point at which a handler trap occurred.  This is a
 * standard two-argument function, thus the closure is in ml_cont (%stdcont).
 */
ENTRY(sigh_resume)
	mov	REQ_SIG_RESUME,ATMP1
	br	set_request

/* pollh_return_a
 * The return continuation for the ML poll handler.
 */
ML_CODE_HDR(pollh_return_a)
	mov	REQ_POLL_RESUME,ATMP1
	mov	ML_unit, STDLINK
	mov	ML_unit, STDCLOS
	mov	ML_unit, PC
	br	set_request

/* pollh_resume:
 * Resume execution at the point at which a poll event occurred.
 */
ENTRY(pollh_resume)
	mov	REQ_POLL_RETURN,ATMP1
	br	set_request

ML_CODE_HDR(handle_a)
	mov	REQ_EXN,ATMP1
	mov	STDLINK, PC
	br	set_request

ML_CODE_HDR(return_a)
	mov	REQ_RETURN,ATMP1
	mov	ML_unit, STDLINK
	mov	ML_unit, STDCLOS
	mov	ML_unit, PC
	br	set_request

ENTRY(request_fault)
	mov	REQ_FAULT,ATMP1
	mov	STDLINK, PC
	br	set_request

/* bind_cfun : (string * string) -> c_function
 */
ML_CODE_HDR(bind_cfun_a)
	CHECKLIMIT
	mov	REQ_BIND_CFUN,ATMP1
	br	set_request

ML_CODE_HDR(build_literals_a)
	CHECKLIMIT
	mov	REQ_BUILD_LITERALS,ATMP1
	br	set_request

ML_CODE_HDR(callc_a)
	CHECKLIMIT
	mov	REQ_CALLC,ATMP1
	br	set_request

	BEGIN_PROC(saveregs)
ENTRY(saveregs)
	mov	REQ_GC,ATMP1

	/* fall through */

set_request:			
	ldq	PTRTMP,MLSTATE_OFFSET(sp)	/* get the ML state ptr */
	ldq	NEEDGC,VProcOffMSP(PTRTMP)	/* use NEEDGC for VProcPtr */
	stl	zero,InMLOffVSP(NEEDGC)		/* note that we have left ML */
	stl	ALLOCPTR,AllocPtrOffMSP(PTRTMP)
	stl	LIMITPTR,LimitPtrOffMSP(PTRTMP)
	stl	STOREPTR,StorePtrOffMSP(PTRTMP)
	stl	STDLINK,LinkRegOffMSP(PTRTMP)
	stl	PC,PCOffMSP(PTRTMP)
	stl	STDARG,StdArgOffMSP(PTRTMP)
	stl	STDCLOS,StdClosOffMSP(PTRTMP)
	stl	STDCONT,StdContOffMSP(PTRTMP)
	stl	VARPTR,VarPtrOffMSP(PTRTMP)
	stl	EXNCONT,ExnPtrOffMSP(PTRTMP)
	mov	ATMP1,CRESULT				/* return request */
	stl	MISCREG0,Misc0OffMSP(PTRTMP)
	stl	MISCREG1,Misc1OffMSP(PTRTMP)
	stl	MISCREG2,Misc2OffMSP(PTRTMP)
	/* fall through */
	.end saveregs

					/* restore callee-save C registers */
restore_c_regs:
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


	BEGIN_PROC(restoreregs)
ENTRY(restoreregs)
	subq	sp,ML_FRAMESIZE			/* allocate a stack frame */
	.frame	sp,ML_FRAMESIZE,zero
	.mask	0xe000fe00,0
						/* save the C registers */
	lda	$3,saveregs
	stq	CARG0,MLSTATE_OFFSET(sp)	/* save MLState ptr for return to C */
	stq	$3,STARTGC_OFFSET(sp)		/* so ML can find saveregs! */
	lda     $3,ml_divlv			/* address of ml_divlv */
	stl	$3,ML_DIVLV_OFFSET(sp)
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
	ldl	MISCREG0,Misc0OffMSP(PTRTMP)
	ldl	MISCREG1,Misc1OffMSP(PTRTMP)
	ldl	MISCREG2,Misc2OffMSP(PTRTMP)
	ldl	STDLINK,LinkRegOffMSP(PTRTMP)
	ldl	PC,PCOffMSP(PTRTMP)
	ldl	VARPTR,VarPtrOffMSP(PTRTMP)
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
	jmp	(PC)			/* jump/return to ML code */
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
	CHECKLIMIT

	ldl	ATMP1,0(STDARG)		    /* tmp1 := length in words */
	sra	ATMP1,1,ATMP2		    /* tmp2 := length (untagged) */
	subl	ATMP2,SMALL_OBJ_SZW,ATMP3   /* is this a small object? */
	bgt	ATMP3,2f		    /* branch if large object */

	ldl	STDARG,4(STDARG)	    /* initial value */
	sll	ATMP2,TAG_SHIFTW,ATMP3	    /* build descriptor in tmp3 */
	or	ATMP3,MAKE_TAG(DTAG_arr_data),ATMP3
	stl	ATMP3,0(ALLOCPTR)	    /* store descriptor */
	addq	ALLOCPTR,4		    /* allocptr++ */
	mov	ALLOCPTR,ATMP3		    /* array data ptr in tmp3 */
1:	
	stl	STDARG,0(ALLOCPTR)	    /* initialize array */ 
	subl	ATMP2, 1, ATMP2
	addq	ALLOCPTR,4
	bne	ATMP2,1b

	/* allocate array header */
	mov	DESC_polyarr,ATMP2	    /* descriptor in tmp2 */
	stl	ATMP2,0(ALLOCPTR)	    /* store descriptor */
	addq	ALLOCPTR, 4		    /* allocptr++ */
	mov	ALLOCPTR,STDARG		    /* result = header addr */
	stl	ATMP3, 0(ALLOCPTR)	    /* store pointer to data */
	stl	ATMP1, 4(ALLOCPTR)
	addq	ALLOCPTR,8
	CONTINUE

2:	/* off-line allocation of big arrays */
	mov	REQ_ALLOC_ARRAY,ATMP1
        mov	STDLINK, PC
	br	set_request

/* create_r : int -> realarray
 * Create a new realarray.
 */
ML_CODE_HDR(create_r_a)
	CHECKLIMIT

	sra	STDARG,1,ATMP2		    /* atmp2 = length (untagged int) */
	sll	ATMP2,1,ATMP2		    /* atmp2 = length in words */
	subl	ATMP2,SMALL_OBJ_SZW,ATMP3
	bgt	ATMP3,1f		    /* is this a small object? */
	/* allocate the data object */
	sll	ATMP2,TAG_SHIFTW,ATMP1	    /* build data descriptor in tmp1 */
	or	ATMP1, MAKE_TAG(DTAG_raw64),ATMP1
#ifdef ALIGN_REALDS
	or	ALLOCPTR,4,ALLOCPTR	    /* tag is unaligned, so that the */
					    /* first element is 8-byte aligned */
#endif
	stl	ATMP1,0(ALLOCPTR)	    /* store the descriptor */
	addq	ALLOCPTR,4		    /* allocptr++ */
	mov	ALLOCPTR,ATMP3		    /* tmp3 = data object */
	sll	ATMP2, 2, ATMP2		    /* tmp2 = length in bytes */
	addq	ALLOCPTR, ATMP2,ALLOCPTR    /* allocptr += length */
	/* allocate the header object */
	mov	DESC_real64arr,ATMP1
	stl	ATMP1,0(ALLOCPTR)	    /* header descriptor */
	addq	ALLOCPTR,4		    /* allocptr++ */
	stl	ATMP3,0(ALLOCPTR)	    /* header data field */
	stl	STDARG,4(ALLOCPTR)	    /* header length field */
	mov	ALLOCPTR,STDARG		    /* stdarg = header object */
	addq	ALLOCPTR,8
	CONTINUE

1:	/* off-line allocation of big realarrays */
	mov	REQ_ALLOC_REALDARRAY,ATMP1
        mov	STDLINK, PC
	br	set_request

/* create_b : int -> bytearray
 * Create a bytearray of the given length.
 */
ML_CODE_HDR(create_b_a)
	CHECKLIMIT

	sra	STDARG,1,ATMP2		  /* atmp2 = length (untagged int) */
	addq	ATMP2,3,ATMP2		  /* atmp2 = length in words */
	sra	ATMP2,2
	subq	ATMP2,SMALL_OBJ_SZW,ATMP3 /* is this a small object? */
	bgt	ATMP3,1f
	/* allocate the data object */
	sll	ATMP2,TAG_SHIFTW,ATMP1    /* build descriptor in atmp1 */
	or	ATMP1,MAKE_TAG(DTAG_raw32),ATMP1
	stl	ATMP1,0(ALLOCPTR)	  /* store the data descriptor */
	addq	ALLOCPTR,4		  /* allocptr++ */
	mov	ALLOCPTR,ATMP3		  /* tmp3 = data object */
	sll	ATMP2,2		          /* tmp2 = length in bytes */
	addq	ALLOCPTR,ATMP2,ALLOCPTR   /* allocptr += total length */
	/* allocate the header object */
	mov	DESC_word8arr,ATMP1	  /* header descriptor */
	stl	ATMP1,0(ALLOCPTR)
	addq	ALLOCPTR,4		  /* allocptr++ */
	stl	ATMP3,0(ALLOCPTR)	  /* header data field */
	stl	STDARG,4(ALLOCPTR)	  /* header length field */
	mov	ALLOCPTR,STDARG		  /* stdarg = header object */
	addq	ALLOCPTR,8		  /* allocptr += 2 */
	CONTINUE
1:					/* off-line allocation of big bytearrays */
	mov	REQ_ALLOC_BYTEARRAY,ATMP1
        mov	STDLINK, PC
	br	set_request

/* create_s : int -> string
 * Create a string of the given length (assume length >0).
 */
ML_CODE_HDR(create_s_a)
	CHECKLIMIT

	sra	STDARG,1,ATMP2			/* tmp2 = length (untagged int) */
	addq	ATMP2,4,ATMP2			/* atmp2 = length in words */
	sra	ATMP2,2
	subq	ATMP2,SMALL_OBJ_SZW,ATMP3
	bgt	ATMP3,1f			/* is this a small object? */
	
	sll	ATMP2,TAG_SHIFTW,ATMP1		/* build descriptor in atmp3 */
	or	ATMP1,MAKE_TAG(DTAG_raw32),ATMP1
	stl	ATMP1,0(ALLOCPTR)		/* store the data descriptor */
	addq	ALLOCPTR,4	 	        /* allocptr++ */
	mov	ALLOCPTR,ATMP3			/* tmp3 = data object */
	sll	ATMP2,2				/* tmp2 = length in bytes */
	addq	ALLOCPTR,ATMP2,ALLOCPTR	        /* allocptr += total length */
	stl	zero,-4(ALLOCPTR)		/* store zero in last word */
	/* Allocate the header object */
	mov	DESC_string, ATMP1		/* header descriptor */
	stl	ATMP1,0(ALLOCPTR)
	addq	ALLOCPTR,4			/* allocptr++ */
	stl	ATMP3,0(ALLOCPTR)		/* header data field */
	stl	STDARG,4(ALLOCPTR)		/* heder length field */
	mov	ALLOCPTR,STDARG			/* stdarg = header object */
	addq	ALLOCPTR,8
	CONTINUE
1:					/* off-line allocation of big strings */
	mov	REQ_ALLOC_STRING,ATMP1
        mov	STDLINK, PC
	br	set_request

/* create_v_a : (int * 'a list) -> 'a vector
 * Create a vector with elements taken from a list.
 * NOTE: the front-end ensures that list cannot be nil.
 */
ML_CODE_HDR(create_v_a)
	CHECKLIMIT

	ldl	ATMP1,0(STDARG)		/* tmp1 := length (tagged int) */
	sra	ATMP1,1,ATMP2		/* tmp2 := length (untagged) */
	subq	ATMP2,SMALL_OBJ_SZW,ATMP3
	bgt	ATMP3,1f		/* is this a small object? */
	
	sll	ATMP2,TAG_SHIFTW,ATMP2  /* build descriptor in tmp2 */
	or	ATMP2,MAKE_TAG(DTAG_vec_data),ATMP2
	stl	ATMP2,0(ALLOCPTR)	/* store descriptor */
	addq	ALLOCPTR,4		/* allocptr++ */
	ldl	ATMP2,4(STDARG)	        /* atmp2 := list */
	mov	ALLOCPTR,STDARG		/* stdarg := vector */
2:     /* loop: */
	ldl	ATMP3,0(ATMP2)	        /* tmp3 := hd(tmp2) */
	ldl	ATMP2,4(ATMP2)	        /* tmp2 := tl(tmp2) */
	stl	ATMP3,0(ALLOCPTR)       /* store word in vector */
	addq	ALLOCPTR,4		/* allocptr++ */
	cmpeq	ATMP2,ML_nil,ATMP3	/* tmp3 := 1 if tmp2=ML_nil */
	beq	ATMP3, 2b
       /* allocate header object */
	mov	DESC_polyvec,ATMP3	/* descriptor in tmp3 */
	stl	ATMP3,0(ALLOCPTR)	/* store descriptor */
	addq	ALLOCPTR,4		/* allocptr++ */
	stl	STDARG,0(ALLOCPTR)	/* header data field */
	stl	ATMP1,4(ALLOCPTR)	/* header length */
	mov	ALLOCPTR, STDARG	/* result = header object */
	addq	ALLOCPTR, 8		/* allocptr += 2 */
	CONTINUE

1:	/* off-line allocation for large vectors */
	mov	REQ_ALLOC_VECTOR,ATMP1
        mov	STDLINK, PC
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
	CHECKLIMIT
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

/* ml_divlv
 * Incoming parameters in $16 and $17, result in $0	
 */
ENTRY(ml_divlv)			/* divide longword */
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

