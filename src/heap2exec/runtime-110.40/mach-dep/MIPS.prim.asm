/* MIPS.prim.asm
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 */

#define _MIPS_SIM	1	/* IRIX 5.x needs this in <regdef.h> */

/** include <regdef.h> **/
#define zero	$0	/* wired zero */
#define AT	$at	/* assembler temp */
#define v0	$2	/* return value */
#define v1	$3
#define a0	$4	/* argument registers */
#define a1	$5
#define a2	$6
#define a3	$7
#define t0	$8	/* caller saved */
#define t1	$9
#define t2	$10
#define t3	$11
#define t4	$12	/* caller saved - 32 bit env arg reg 64 bit */
#define ta0	$12	/* caller saved in 32 bit - arg regs in 64 bit */
#define t5	$13
#define ta1	$13
#define t6	$14
#define ta2	$14
#define t7	$15
#define ta3	$15
#define s0	$16	/* callee saved */
#define s1	$17
#define s2	$18
#define s3	$19
#define s4	$20
#define s5	$21
#define s6	$22
#define s7	$23
#define t8	$24	/* code generator */
#define t9	$25
#define jp	$25	/* PIC jump register */
#define k0	$26	/* kernel temporary */
#define k1	$27
#define gp	$28	/* global pointer */
#define sp	$29	/* stack pointer */
#define fp	$30	/* frame pointer */
#define s8	$30	/* calle saved */
#define ra	$31	/* return address */
/** end <regdef.h> **/

#include "ml-base.h"
#include "asm-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-request.h"
#include "reg-mask.h"
#include "ml-limits.h"
#include "mlstate-offsets.h"	/** this file is generated **/

/* MIPS runtime code for ML. The MIPS registers are used as follows; the names
 * in parentheses are the MLState field names (see ml_state.h):
 *
 *    register  C callee	ML use
 *		  save?
 *    --------  ---------       -------
 *	$1	   no		assembler temporary
 *	$2	   no		standard arg
 *	$3	   no		standard continuation
 *	$4	   no		standard closure
 *	$5	   no		standard link register
 *	$6-$15	   no		miscellaneous registers
 *	$16-$18	   yes		miscellaneous registers
 *	$19	   yes		heap limit pointer
 *	$20	   yes		var pointer
 *	$21	   yes		heap-limit comparison flag, and arith temporary
 *	$22	   yes		store list pointer
 *	$23	   yes		allocation pointer
 *	$24	   no		base address register
 *	$25	    -		internal temporary (ptrtmp)
 *	$26	    -		reserved for operating system
 *	$27	    -		reserved for operating system
 *	$28	    -		reserved for C (global pointer)
 *	$29	    -		reserved for C (stack pointer)
 *	$30	   yes		exception continuation
 *	$31	   no		gc link register
 */
/* assembler-temp  $1  							 */
#define     stdarg $2 	/*  	standard arg  (ml_arg)		 	  */
#define    stdcont $3 	/*      standard continuation (ml_cont) 	  */
#define    stdclos $4 	/*      standard closure (ml_closure)             */
#define    stdlink $5	/* 	ptr to just-entered std function (ml_link) */
#define   miscreg0 $6   /*  miscellaneous registers, 0..12 (ml_roots[]) */
#define   miscreg1 $7
#define   miscreg2 $8
#define   miscreg3 $9
#define  miscreg4 $10
#define  miscreg5 $11
#define  miscreg6 $12
#define  miscreg7 $13
#define  miscreg8 $14
#define  miscreg9 $15
#define miscreg10 $16
#define miscreg11 $17
#define miscreg12 $18
#define     limit $19 	/*      end of heap - 4096  (ml_limitptr)  	   */
#define    varptr $20 	/*      per-thread var pointer (ml_varptr) */
#define exhausted $21 	/*      arith temp; also, heap-limit comparison flag */
#define  storeptr $22 	/*   	store pointer  (ml_storeptr) 		*/
#define  allocptr $23 	/* 	freespace pointer  (ml_allocptr) */
#define   basereg $24 	/*       pointer to base of code object+32764 (ml_roots[]) */
#define    ptrtmp $25 	/*       internal temporary */
/*                $26		reserved for operating system     */
/*                $27		reserved for operating system     */
/*  globalptr     $28		reserved for C and assembler */
/*  stackptr      $29          stack pointer 			*/
#define   exncont $30 	/*       exception handler (ml_exncont) */
#define    gclink $31 	/* 	resumption point for restoreregs (ml_pc) */

#define atmp1 miscreg9
#define atmp2 miscreg10
#define atmp3 miscreg11
#define atmp4 miscreg12

/** register names for polling **/
#define pfreq	exhausted
#define	pevent	miscreg0
#define ptmp    miscreg1

#define cresult	$2
#define carg0	$4
#define carg1	$5

/* The root registers in the ML state vector have the following layout,
 * where roots is guaranteed to be 8-byte aligned relative to the start
 * of the ML state vector (see "ml-state.h" and "mlstate-offsets.h"):
 *
 *			+-------------------+
 *	roots:   	|    ml_arg ($2)    |
 *			+-------------------+
 *	roots+4: 	|    ml_cont ($3)   |
 *			+-------------------+
 *	roots+8: 	|  ml_closure ($4)  |
 *			+-------------------+
 *      roots+12:	| ml_exncont ($30)  |
 *			+-------------------+
 *	roots+16:	|       ml_pc       |
 *			+-------------------+
 *	roots+20:	|  ml_linkReg (%5)  |
 *			+-------------------+
 *	roots+24:	|     ($6-$18)      |
 *			+-------------------+
 *	roots+76:	|  ml_varReg ($20)  |
 *			+-------------------+
 *	roots+48:	|  ml_baseReg ($24) |
 *			+-------------------+
 */


/* The  ML stack frame has the following layout (set up by restoreregs):
 * NOTE: this is backwards from the MIPS convention, the register save
 * area should be below the locals!!!
 *
 * NOTE: the offset info for pseudo regs and saveregs is used in 
 *       mips/mips.sml
 *
 *			+-------------------+
 *      sp+60:		|     saved $31     |
 *			+-------------------+
 *      sp+56:		|     saved $30     |
 *			+-------------------+
 *      sp+52:		|     saved $23     |
 *			+-------------------+
 *      sp+48:		|     saved $22     |
 *			+-------------------+
 *      sp+44:		|     saved $21     |
 *			+-------------------+
 *      sp+40:		|     saved $20     |
 *			+-------------------+
 *      sp+36:		|     saved $19     |
 *			+-------------------+
 *      sp+32:		|     saved $18     |
 *			+-------------------+
 *      sp+28:		|     saved $17     |
 *			+-------------------+
 *      sp+24:		|     saved $16     |
 *			+-------------------+
 *      sp+20:          |   pseudo reg 2    |
 *			+-------------------+
 *      sp+16:          |   pseudo reg 1    |
 *			+-------------------+
 *	sp+8:		|      unused	    |
 *			+-------------------+
 *	sp+4:		| addr of saveregs  |
 *			+-------------------+
 *      sp:		|  ptr to MLState   |
 *			+-------------------+
 */

#define MLSTATE_OFFSET	0
#define STARTGC_OFFSET	4
#define REGSAVE_OFFSET	24
#define PSEUDOREG_OFFSET 16
#define ML_FRAMESIZE	4096


#ifdef MIPS2
#  define LOAD_DELAY_SLOT
#else
#  define LOAD_DELAY_SLOT	nop
#endif


#if (CALLEESAVE > 0)
#define CONTINUE						\
	    sltu	exhausted,allocptr,limit;		\
            j		stdcont;
#else
#define CONTINUE						\
	    lw		stdlink,0(stdcont);			\
	    sltu	exhausted,allocptr,limit;		\
	    j		stdlink
#endif

#define CHECKLIMIT(mask)					\
	    bnez	exhausted,3f;				\
	    li		ptrtmp,mask;				\
	    move	gclink,stdlink;				\
	    b		saveregs;				\
	 3:


	.text

/* sigh_return_a:
 * The return continuation for the ML signal handler.
 */
ML_CODE_HDR(sigh_return_a)
	li	ptrtmp,RET_MASK
	li	atmp1,REQ_SIG_RETURN
	b	set_request

/* sigh_resume:
 * Resume execution at the point at which a handler trap occurred.  This is a
 * standard two-argument function, thus the closure is in ml_cont (%stdcont).
 */
ENTRY(sigh_resume)
	li	ptrtmp,RET_MASK
	li	atmp1,REQ_SIG_RESUME
	b	set_request

/* pollh_return_a:
 * The return continuation for the ML poll handler.
 */
ML_CODE_HDR(pollh_return_a)
	li	ptrtmp,RET_MASK
	li	atmp1,REQ_POLL_RETURN
	b	set_request

/* pollh_resume:
 * Resume execution at the point at which a poll event occurred.
 */
ENTRY(pollh_resume)
	li	ptrtmp,RET_MASK
	li	atmp1,REQ_POLL_RESUME
	b	set_request

ML_CODE_HDR(handle_a)
	li	ptrtmp,EXN_MASK
	li	atmp1,REQ_EXN
	b	set_request

ML_CODE_HDR(return_a)
	li	ptrtmp,RET_MASK
	li	atmp1,REQ_RETURN
	b	set_request

ENTRY(request_fault)
	li	ptrtmp,EXN_MASK
	li	atmp1,REQ_FAULT
	b	set_request

/* bind_cfun : (string * string) -> c_function
 */
ML_CODE_HDR(bind_cfun_a)
	CHECKLIMIT(FUN_MASK)
	li	ptrtmp,FUN_MASK
	li	atmp1,REQ_BIND_CFUN
	b	set_request

ML_CODE_HDR(build_literals_a)
	CHECKLIMIT(FUN_MASK)
	li	ptrtmp,FUN_MASK
	li	atmp1,REQ_BUILD_LITERALS
	b	set_request

ML_CODE_HDR(callc_a)
	CHECKLIMIT(FUN_MASK)
	li	ptrtmp,FUN_MASK
	li	atmp1,REQ_CALLC
	/* fall through */

set_request:				/* a quick return to run_ml(), ptrtmp holds */
					/* the request code, and atmp1 holds the  */
					/* live register mask. */

	move	exhausted,ptrtmp		/* save the register mask */
	lw	ptrtmp,MLSTATE_OFFSET(sp)	/* get the ML state ptr from the stack */
	sw	exhausted,MaskOffMSP(ptrtmp)
	lw	exhausted,VProcOffMSP(ptrtmp)	/* fetch the VProc state ptr */
	sw	zero,InMLOffVSP(exhausted)	/* note that we have left ML */
	sw	allocptr,AllocPtrOffMSP(ptrtmp)
	sw	storeptr,StorePtrOffMSP(ptrtmp)
	sw	stdlink,LinkRegOffMSP(ptrtmp)
	sw	stdlink,PCOffMSP(ptrtmp)	/* address of called function */
	sw	stdarg,StdArgOffMSP(ptrtmp)
	sw	stdclos,StdClosOffMSP(ptrtmp)
	sw	stdcont,StdContOffMSP(ptrtmp)
	sw	varptr,VarPtrOffMSP(ptrtmp)
	sw	exncont,ExnPtrOffMSP(ptrtmp)
	move	cresult,atmp1			/* return request */
#if (CALLEESAVE > 0)
	sw	miscreg0,MiscRegOffMSP(0)(ptrtmp)
#if (CALLEESAVE > 1)
	sw	miscreg1,MiscRegOffMSP(1)(ptrtmp)
#if (CALLEESAVE > 2)
	sw	miscreg2,MiscRegOffMSP(2)(ptrtmp)
#if (CALLEESAVE > 3)
	sw	miscreg3,MiscRegOffMSP(3)(ptrtmp)
#if (CALLEESAVE > 4)
	sw	miscreg4,MiscRegOffMSP(4)(ptrtmp)
#if (CALLEESAVE > 5)
	sw	miscreg5,MiscRegOffMSP(5)(ptrtmp)
#if (CALLEESAVE > 6)
	sw	miscreg6,MiscRegOffMSP(6)(ptrtmp)
#if (CALLEESAVE > 7)
	sw	miscreg7,MiscRegOffMSP(7)(ptrtmp)
#if (CALLEESAVE > 8)
	sw	miscreg8,MiscRegOffMSP(8)(ptrtmp)
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
					/* restore callee-sace C registers */
restore_c_regs:
	lw	atmp1,PSEUDOREG_OFFSET(sp)
	lw	atmp2,PSEUDOREG_OFFSET+4(sp)
	sw	atmp1,PseudoReg1OffMSP(ptrtmp)
	sw	atmp2,PseudoReg2OffMSP(ptrtmp)
	lw	$31,REGSAVE_OFFSET+36(sp)
	lw	$30,REGSAVE_OFFSET+32(sp)
        lw      $23,REGSAVE_OFFSET+28(sp)
        lw      $22,REGSAVE_OFFSET+24(sp)
        lw      $21,REGSAVE_OFFSET+20(sp)
        lw      $20,REGSAVE_OFFSET+16(sp)
        lw      $19,REGSAVE_OFFSET+12(sp)
        lw      $18,REGSAVE_OFFSET+8(sp)
        lw      $17,REGSAVE_OFFSET+4(sp)
        lw      $16,REGSAVE_OFFSET(sp)
	addu	sp,ML_FRAMESIZE			/* discard the stack frame */
	j	$31				/* return to run_ml() */

	BEGIN_PROC(saveregs)
ENTRY(saveregs)
	move	exhausted,ptrtmp		/* save the register mask */
	lw	ptrtmp,MLSTATE_OFFSET(sp)	/* use ptrtmp to access ML state */
	sw	exhausted,MaskOffMSP(ptrtmp)

#ifdef SOFT_POLL
/** NOTE: why not use tmp registers here instead of misc regs?? **/
      /* free some regs */
	sw	miscreg0,MiscRegOffMSP(0)(ptrtmp)	/* use as tmp */
	sw	miscreg1,MiscRegOffMSP(1)(ptrtmp)	/* use as tmp */
#define pfreq	exhausted
#define	pevent	miscreg0
#define tmp     miscreg1

      /* check if polling enabled (poll_freq > 0) */
	la	pfreq,CSYM(_PollFreq0)		/* load contents of ref */
	lw	pfreq,4(pfreq)
	srl	pfreq,1				/* strip integer tag */
	beqz	pfreq,check_for_gc		/* go check for real gc */
	lw	tmp,InPollHandlerOffMSP(ptrtmp)	/* if we're in the handler */
	bnez	tmp,reset_limit			/* ignore poll events */
	la	tmp,CSYM(_PollEvent0)		/* load contents of ref */
	lw	pevent,4(tmp)
	srl	pevent,1
	beqz	pevent,reset_limit
      /* event occurred, so set ml_pollHandlerPending */
	li	tmp,1
	sw	tmp,PollPendingOffMSP(ptrtmp)
	b	do_gc		/* and handle event in the C runtime */

reset_limit:				/* reset limit ptr */
	sll	pfreq,POLL_GRAIN_BITS		/* mult by POLL_GRAIN_CPSI */
	addu	limit,pfreq,allocptr		/* overflow handled below */

check_for_gc:
#define	tmp2	pfreq
#define vsp	pevent
	/* ensure real limit is >= limit */
	lw	tmp,RealLimitOffMSP(ptrtmp)
	sltu	tmp2,tmp,limit
	beqz	tmp2,ok_limit		/* (tmp >= limit) */
	move	limit,tmp
ok_limit:
	subu	tmp,limit,allocptr
	slt	tmp,tmp,4096
	bnez	tmp,do_gc   	       	/* gc *//* should be a common case */
	/* since a signal also sets limitptr == allocptr to force a trap, */
	/* we need to disambiguate poll-events/signals here */
	lw	vsp,VProcOffMSP(ptrtmp)
	lw	tmp,PollPendingOffMSP(ptrtmp)
	lw	tmp2,NPendingOffVSP(vsp)
	add	tmp,tmp,tmp2
	lw	tmp2,NPendingSysOffVSP(vsp)
	add	tmp,tmp,tmp2
	bnez	tmp2,do_gc
#undef  pfreq
#undef  pevent
#undef  vsp
#undef  tmp2

no_gc:	/* an uneventful poll check, back to ML */
	lw	miscreg0,MiscRegOffMSP(0)(ptrtmp)  /* reload miscregs */
	lw	miscreg1,MiscRegOffMSP(1)(ptrtmp)
.set	noreorder
	j	gclink
	sltu	exhausted,allocptr,limit	/* (delay slot) */
.set 	reorder
		
do_gc:
	sw	limit,LimitPtrOffMSP(ptrtmp)
#endif /* SOFT_POLL */

	lw	exhausted,VProcOffMSP(ptrtmp)
	sw	zero,InMLOffVSP(exhausted)	/* note that we have left ML */
	sub     basereg,32764			/* adjust baseReg */
	sw	allocptr,AllocPtrOffMSP(ptrtmp)
	sw	storeptr,StorePtrOffMSP(ptrtmp)
	sw	stdarg,StdArgOffMSP(ptrtmp)
	sw	stdcont,StdContOffMSP(ptrtmp)
	sw	stdclos,StdClosOffMSP(ptrtmp)
	sw	gclink,PCOffMSP(ptrtmp)
	sw	exncont,ExnPtrOffMSP(ptrtmp)
						/* save misc. roots */
#ifndef SOFT_POLL  /* miscreg0 & miscreg1 saved above for SOFT_POLL */
	sw	miscreg0,MiscRegOffMSP(0)(ptrtmp)
	sw	miscreg1,MiscRegOffMSP(1)(ptrtmp)
#endif
	sw	miscreg2,MiscRegOffMSP(2)(ptrtmp)
	sw	miscreg3,MiscRegOffMSP(3)(ptrtmp)
	sw	miscreg4,MiscRegOffMSP(4)(ptrtmp)
	sw	miscreg5,MiscRegOffMSP(5)(ptrtmp)
	sw	miscreg6,MiscRegOffMSP(6)(ptrtmp)
	sw	miscreg7,MiscRegOffMSP(7)(ptrtmp)
	sw	miscreg8,MiscRegOffMSP(8)(ptrtmp)
	sw	miscreg9,MiscRegOffMSP(9)(ptrtmp)
	sw	miscreg10,MiscRegOffMSP(10)(ptrtmp)
	sw	miscreg11,MiscRegOffMSP(11)(ptrtmp)
	sw	miscreg12,MiscRegOffMSP(12)(ptrtmp)
	sw	stdlink,LinkRegOffMSP(ptrtmp)
	sw	basereg,BasePtrOffMSP(ptrtmp)		/* base reg */
	sw	varptr,VarPtrOffMSP(ptrtmp)
	li	cresult,REQ_GC
	b	restore_c_regs
	.end	saveregs


	BEGIN_PROC(restoreregs)
ENTRY(restoreregs)
	subu	sp,ML_FRAMESIZE			/* allocate a stack frame */
	.frame	sp,ML_FRAMESIZE,zero
	.mask	0xc0ff0000,0
						/* save the C registers */
	la	$5,saveregs
	sw	carg0,MLSTATE_OFFSET(sp)	/* save MLState ptr for return to C */
	sw	$5,STARTGC_OFFSET(sp)		/* so ML can find saveregs! */
	sw	$31,REGSAVE_OFFSET+36(sp)
	sw	$30,REGSAVE_OFFSET+32(sp)
        sw      $23,REGSAVE_OFFSET+28(sp)
        sw      $22,REGSAVE_OFFSET+24(sp)
        sw      $21,REGSAVE_OFFSET+20(sp)
        sw      $20,REGSAVE_OFFSET+16(sp)
        sw      $19,REGSAVE_OFFSET+12(sp)
        sw      $18,REGSAVE_OFFSET+8(sp)
        sw      $17,REGSAVE_OFFSET+4(sp)
        sw      $16,REGSAVE_OFFSET(sp)
	move    ptrtmp,carg0			/* put MLState ptr in ptrtmp */

	lw	atmp1,PseudoReg1OffMSP(ptrtmp) /* save pseudo registers */
	lw	atmp2,PseudoReg2OffMSP(ptrtmp)
	sw	atmp1,PSEUDOREG_OFFSET(sp)
	sw	atmp2,PSEUDOREG_OFFSET+4(sp)

	lw	allocptr,AllocPtrOffMSP(ptrtmp)
	lw	limit,LimitPtrOffMSP(ptrtmp)
	lw	storeptr,StorePtrOffMSP(ptrtmp)
	li	atmp1,1
	lw	exhausted,VProcOffMSP(ptrtmp)
.set	noreorder			 /* the order here is important */
	sw	atmp1,InMLOffVSP(exhausted) /* note that we are entering ML code */
	lw	stdarg,StdArgOffMSP(ptrtmp)
	lw	stdcont,StdContOffMSP(ptrtmp)
	lw	stdclos,StdClosOffMSP(ptrtmp)
	lw	exncont,ExnPtrOffMSP(ptrtmp)
	lw	miscreg0,MiscRegOffMSP(0)(ptrtmp)
	lw	miscreg1,MiscRegOffMSP(1)(ptrtmp)
	lw	miscreg2,MiscRegOffMSP(2)(ptrtmp)
	lw	miscreg3,MiscRegOffMSP(3)(ptrtmp)
	lw	miscreg4,MiscRegOffMSP(4)(ptrtmp)
	lw	miscreg5,MiscRegOffMSP(5)(ptrtmp)
	lw	miscreg6,MiscRegOffMSP(6)(ptrtmp)
	lw	miscreg7,MiscRegOffMSP(7)(ptrtmp)
	lw	miscreg8,MiscRegOffMSP(8)(ptrtmp)
	lw	miscreg9,MiscRegOffMSP(9)(ptrtmp)
	lw	miscreg10,MiscRegOffMSP(10)(ptrtmp)
	lw	miscreg11,MiscRegOffMSP(11)(ptrtmp)
	lw	miscreg12,MiscRegOffMSP(12)(ptrtmp)
	lw	stdlink,LinkRegOffMSP(ptrtmp)
	lw	varptr,VarPtrOffMSP(ptrtmp)
	lw 	basereg,BasePtrOffMSP(ptrtmp)
	lw	gclink,PCOffMSP(ptrtmp)
	add     basereg,32764			/* adjust baseReg */
						/* check for pending signals */
	lw	ptrtmp,NPendingSysOffVSP(exhausted)
.set	noat
	lw	$1,NPendingOffVSP(exhausted)
	LOAD_DELAY_SLOT
	add	ptrtmp,ptrtmp,$1
.set	at
	bnez	ptrtmp,pending_sigs
	nop					/* (branch delay slot) */
	.end	restoreregs
	.ent	ml_go
ENTRY(ml_go)
	j	gclink				/* jump to ML code */
	sltu	ptrtmp,allocptr,limit	/* (delay slot) */
	.end	ml_go

pending_sigs:	/* there are pending signals */
					/* check if we are currently handling a signal */
	lw	ptrtmp,InSigHandlerOffVSP(exhausted)
	LOAD_DELAY_SLOT
	bnez	ptrtmp,ml_go	
					/* note that a handler trap is pending */
	li	ptrtmp,1
	sw	ptrtmp,HandlerPendingOffVSP(exhausted)
	b	ml_go
	move	limit,allocptr		/* (delay slot) */
.set	reorder


/* SaveFPRegs:
 *
 *   void SaveFPRegs (Word_t *p)
 *
 * Save the Callee-save FP registers starting at the given address.
 */
	TEXT
	BEGIN_PROC(SaveFPRegs)
ENTRY(SaveFPRegs)
	swc1	$f20,4(a0)		/* fpr20 */
	swc1	$f21,8(a0)
	swc1	$f22,12(a0)		/* fpr22 */
	swc1	$f23,16(a0)
	swc1	$f24,20(a0)		/* fpr24 */
	swc1	$f25,24(a0)
	swc1	$f26,28(a0)		/* fpr26 */
	swc1	$f27,32(a0)
	swc1	$f28,36(a0)		/* fpr28 */
	swc1	$f29,40(a0)
	j 	ra			/* return */
	END_PROC(SaveFPRegs)

/* RestoreFPRegs:
 *
 *   void RestoreFPRegs (Word_t *p)
 *
 * Restore the callee-save FP registers from the given address.
 */
	BEGIN_PROC(RestoreFPRegs)
ENTRY(RestoreFPRegs)			/* floats address passed as parm */
	lwc1	$f20,0(a0)		/* retrieve float registers */
	lwc1	$f21,4(a0)
	lwc1	$f22,8(a0)
	lwc1	$f23,12(a0)
	lwc1	$f24,16(a0)
	lwc1	$f25,20(a0)
	lwc1	$f26,24(a0)
	lwc1	$f27,28(a0)
	lwc1	$f28,32(a0)
	lwc1	$f29,36(a0)
	j	ra
	END_PROC(RestoreFPRegs)


/** Primitive object allocation routines **/

/* array : (int * 'a) -> 'a array
 * Allocate and initialize a new array.	 This can cause GC.
 */
ML_CODE_HDR(array_a)
	CHECKLIMIT(FUN_MASK)
	lw	atmp1,0(stdarg)		    /* atmp1 := length (tagged int) */
	sra	atmp1,1			    /* atmp1 := length (untagged int) */
	bgt	atmp1,SMALL_OBJ_SZW,1f	    /* is this a small object? */
	sll	atmp3,atmp1,TAG_SHIFTW      /* build descriptor in atmp3 */
	ori	atmp3,MAKE_TAG(DTAG_array)
	sw	atmp3,0(allocptr)	    /* store descriptor */
	addi	allocptr,4		    /* allocptr++ */
	lw	atmp2,4(stdarg)		    /* atmp2 := initial value */
	move	stdarg,allocptr
	sll	atmp1,2			    /* atmp1 := length in bytes */
	add	atmp1,allocptr		    /* atmp1 is end of array */
2:					    /* loop: */
	sw	atmp2,0(allocptr)		/* store next element */
	addi	allocptr,4			/* allocptr++ */
	bne	allocptr,atmp1,2b		/* if (allocptr != end) goto loop */
					    /* end loop */
	CONTINUE

1:	/* off-line allocation of big arrays */
	li	ptrtmp,FUN_MASK
	li	atmp1,REQ_ALLOC_ARRAY
	b	set_request

/* create_r : int -> realarray
 * Create a new realarray.
 */
ML_CODE_HDR(create_r_a)
	CHECKLIMIT(FUN_MASK)
	sra	atmp1,stdarg,1		    /* atmp1 = length (untagged int) */
	sll	atmp2,atmp1,1		    /* atmp1 = length in words */
	bgt	atmp2,SMALL_OBJ_SZW,1f	    /* is this a small object? */
	sll	atmp3,atmp1,TAG_SHIFTW      /* build descriptor in atmp3 */
	ori	atmp3,atmp3,MAKE_TAG(DTAG_realdarray)
#ifdef ALIGN_REALDS
	ori	allocptr,4		    /* tag is unaligned, so that the */
					    /* first element is 8-byte aligned */
#endif
	sw	atmp3,0(allocptr)
	addi	stdarg,allocptr,4	    /* pointer to new realarray */
	sll	atmp2,2			    /* atmp2 = length in bytes (no tag) */
	addi	atmp2,4			    /* plus tag */
	add	allocptr,atmp2		    /* allocptr += total length */
	CONTINUE

1:	/* off-line allocation of big realarrays */
	li	ptrtmp,FUN_MASK
	li	atmp1,REQ_ALLOC_REALDARRAY
	b	set_request

/* create_b : int -> bytearray
 * Create a bytearray of the given length.
 */
ML_CODE_HDR(create_b_a)
	CHECKLIMIT(FUN_MASK)
	sra	atmp1,stdarg,1		    /* atmp1 = length (untagged int) */
	addi	atmp2,atmp1,3		    /* atmp2 = length in words */
	sra	atmp2,2
	bgt	atmp2,SMALL_OBJ_SZW,1f	    /* is this a small object? */
	sll	atmp3,atmp1,TAG_SHIFTW     /* build descriptor in atmp3 */
	ori	atmp3,atmp3,MAKE_TAG(DTAG_bytearray)
	sw	atmp3,0(allocptr)
	addi	stdarg,allocptr,4	    /* pointer to new bytearray */
	sll	atmp2,2			    /* atmp2 = length in bytes (no tag) */
	addi	atmp2,4			    /* plus tag */
	add	allocptr,atmp2		    /* allocptr += total length */
	CONTINUE
1:					/* off-line allocation of big bytearrays */
	li	ptrtmp,FUN_MASK
	li	atmp1,REQ_ALLOC_BYTEARRAY
	b	set_request

/* create_s : int -> string
 * Create a string of the given length (assume length >0); the string
 * is guaranteed to have at least one 0 byte as termination (which is
 * not counted in its length).
 */
ML_CODE_HDR(create_s_a)
	CHECKLIMIT(FUN_MASK)
	sra	atmp1,stdarg,1		    /* atmp1 = length (untagged int) */
	addi	atmp2,atmp1,4		    /* atmp2 = length in words */
	sra	atmp2,2
	bgt	atmp2,SMALL_OBJ_SZW,1f	    /* is this a small object? */
	sll	atmp3,atmp1,TAG_SHIFTW     /* build descriptor in atmp3 */
	ori	atmp3,atmp3,MAKE_TAG(DTAG_string)
	sw	atmp3,0(allocptr)
	addi	stdarg,allocptr,4	    /* pointer to new string */
	sll	atmp2,2			    /* atmp2 = length in bytes (no tag) */
	addi	atmp2,4			    /* plus tag */
	add	allocptr,atmp2		    /* allocptr += total length */
	sw	zero,-4(allocptr)	    /* store zero in last word */
	CONTINUE
1:					/* off-line allocation of big strings */
	li	ptrtmp,FUN_MASK
	li	atmp1,REQ_ALLOC_STRING
	b	set_request

/* create_v_a : (int * 'a list) -> 'a vector
 * Create a vector with elements taken from a list.
 * NOTE: the front-end ensures that list cannot be nil.
 */
ML_CODE_HDR(create_v_a)
	CHECKLIMIT(FUN_MASK)
	lw	atmp1,0(stdarg)		    /* atmp1 := length (tagged int) */
	sra	atmp1,1			    /* atmp1 := length (untagged) */
	bgt	atmp1,SMALL_OBJ_SZW,1f	    /* is this a small object? */
	sll	atmp2,atmp1,TAG_SHIFTW     /* build descriptor in atmp2 */
	ori	atmp2,MAKE_TAG(DTAG_vector)
	sw	atmp2,0(allocptr)	    /* store descriptor */
	addi	allocptr,4		    /* allocptr++ */
	lw	atmp2,4(stdarg)		    /* atmp2 := list */
	move	stdarg,allocptr		    /* stdarg := vector */
	li	atmp3,ML_nil
2:					    /* loop: */
	lw	atmp1,0(atmp2)		        /* atmp1 := hd(atmp2) */
	lw	atmp2,4(atmp2)		        /* atmp2 := tl(atmp2) */
	sw	atmp1,0(allocptr)	        /* store word in vector */
	addi	allocptr,4		        /* allocptr++ */
	bne	atmp2,atmp3,2b			/* if (atmp2 <> nil) goto loop */
					    /* end loop */
	CONTINUE

1:	/* off-line allocation for large vectors */
	li	ptrtmp,FUN_MASK
	li	atmp1,REQ_ALLOC_VECTOR
	b	set_request

#ifdef MIPSEL
#define BIGPART 4
#else
#define BIGPART 0
#endif
#define LITTLEPART (4-BIGPART)

/* Floating exceptions raised (assuming ROP's are never passed to functions):
 *	DIVIDE BY ZERO - (div)
 *	OVERFLOW/UNDERFLOW - (add,div,sub,mul) as appropriate
 *
 * floor does not check for out-of-range ;  it's up to the ML code to do that beforehand.
 */
ML_CODE_HDR(floor_a)
	CHECKLIMIT(FUN_MASK)
	lwc1	$f4,LITTLEPART(stdarg)	/* get least significant word */
	lwc1	$f5,BIGPART(stdarg)	/* get most significant word */
	cfc1	atmp3,$31		/* grab fpa control register */
	ori	atmp2,atmp3,0x03	/* set rounding bits to 11 */
	ctc1	atmp2,$31		/* return fpa control register */
	cvt.w.d $f6,$f4			/* convert to integer */
	ctc1	atmp3,$31		/* return fpa control register */
	mfc1	stdarg,$f6		/* get in std argument register */
	add	stdarg,stdarg		/* make room for tag bit */
	add	stdarg,1		/* add the tag bit */
	CONTINUE


ML_CODE_HDR(logb_a)
	CHECKLIMIT(FUN_MASK)
	lw 	stdarg,BIGPART(stdarg)	/* most significant part */
	srl 	stdarg,20		/* throw out 20 low bits */
	andi	stdarg,0x07ff		/* clear all but 11 low bits */
	sub 	stdarg,1023		/* subtract 1023 */
	sll 	stdarg,1		/* make room for tag bit */
	add	stdarg,1		/* add the tag bit */
	CONTINUE

ML_CODE_HDR(scalb_a)
	CHECKLIMIT(FUN_MASK)
	lw 	atmp1,4(stdarg)		/* get tagged n */
	sra	atmp1,1			/* get real n */
	beqz	atmp1,9f		/* if zero, return the old float */
	lw	ptrtmp,0(stdarg)	/* get pointer to float */
	lw 	atmp2,BIGPART(ptrtmp)	/* most significant part */
	srl 	atmp2,20		/* throw out 20 low bits */
	andi	atmp2,0x07ff		/* clear all but 11 low bits */
	add	atmp3,atmp2,atmp1	/* new := old + n */
	blt	atmp3,1,under		/* punt if underflow */
	bgt	atmp3,2046,over		/* or overflow */
	xor	atmp3,atmp2		/* at3 = new xor old */
	sll	atmp3,20		/* put exponent in right position */
	lw	atmp2,BIGPART(ptrtmp)	/* most significant word */
	xor	atmp2,atmp3		/* change to new exponent */
	sw	atmp2,BIGPART+4(allocptr)	/* save */
	lw 	atmp2,LITTLEPART(ptrtmp) /* get least significant word */
	sw	atmp2,LITTLEPART+4(allocptr)	/* save lsw */
8:	li	atmp4,DESC_reald        /* make descriptor */
	sw	atmp4,0(allocptr)	/* save descriptor */
	add	stdarg,allocptr,4	/* get pointer to new float */
	add	allocptr,12		/* point to new free word */
        CONTINUE

9:	lw	stdarg,0(stdarg)	/* get old float */
	CONTINUE

over:	li	atmp3,0x7fffffff
	add	atmp3,atmp3		/* generate overflow exception */

under:	sw	zero,4(allocptr)		/* return 0.0 */
	sw	zero,8(allocptr)
	b	8b

/* try_lock : spin_lock -> bool
 * low-level test-and-set style primitive for mutual-exclusion among 
 * processors.
 */
ML_CODE_HDR(try_lock_a)
	CHECKLIMIT(FUN_MASK)
#ifdef MP_SUPPORT
#  if defined(OPSYS_IRIX5)
	ll	atmp1,0(stdarg)
	srl	atmp1,1
	bnez	atmp1,isset
	li	atmp2,ML_true
	sc	atmp2,0(stdarg)
	beqz	atmp2,isset	
	li	stdarg,ML_true
1:
	CONTINUE
isset:
	li	stdarg,ML_false
	b	1b
#  else
#    error no MP support for this system
#  endif
#else /* ! MP_SUPPORT */
	lw	atmp1,0(stdarg)
	li	atmp2,ML_true
	sw	atmp2,0(stdarg)
	move	stdarg,atmp2
	CONTINUE
#endif

/* unlock : spin_lock -> unit
 * Release an ML spin lock.
 */
ML_CODE_HDR(unlock_a)
	CHECKLIMIT(FUN_MASK)
	li	atmp1,ML_false
	sw	atmp1,0(stdarg)
	li	stdarg,ML_unit
	CONTINUE

/** C callable routines for managing ML locks **/
#ifdef MP_SUPPORT
#  ifdef OPSYS_IRIX5
/* LOCK_Lock:
 *
 *    bool_t LOCK_Lock (ml_state_t *msp, ml_val_t lock)
 */
	BEGIN_PROC(LOCK_Lock)
ENTRY(LOCK_Lock)
	ll	t0,0(carg1)
	srl	t0,1
	bnez	t0,1f
	li	t1,ML_true
	sc	t1,0(carg1)
	beqz	t1,1f
	li	cresult,TRUE
	j	ra
1:
	li	cresult,FALSE
	j	ra
	END_PROC(LOCK_Lock)

/* LOCK_Unlock:
 *
 *    void LOCK_Unlock (ml_state_t *msp, ml_val_t lock)
 */
	BEGIN_PROC(LOCK_Unlock)
ENTRY(LOCK_Unlock)
	li	t0,ML_false
	sw	t0,0(carg1)
	j	ra
	END_PROC(LOCK_Unlock)
#  else
#    error MP not supported for system
#  endif
#endif /* MP_SUPPORT */

/* SetFSR:
 * Turn on floating-point overflow, underflow and zero-divide exceptions.
 */
	BEGIN_PROC(SetFSR)
ENTRY(SetFSR)
	cfc1	t0,$31		/* grab fpa control register */
	ori 	t0,t0,0x00	/* disable all flags */
	ctc1	t0,$31		/* return fpa control register */
	j	ra
	END_PROC(SetFSR)


#ifdef OPSYS_MACH

/* ReenableFP:
 * MACH based DECstations .don not handle SIGFPE correctly, so we have to
 * execute a FP instruction to clear things up.
 */
	BEGIN_PROC(ReenableFP)
ENTRY(ReenableFP)
	mfc1	atmp1,$f1
	j	$31
	END_PROC(ReenableFP)

#endif /* OPSYS_MACH */
