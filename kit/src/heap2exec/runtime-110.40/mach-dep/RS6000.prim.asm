/* RS6000.prim.asm
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 */

#ifndef _ASM_
#define _ASM_
#endif

#include "ml-base.h"
#include "asm-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-request.h"
#include "reg-mask.h"
#include "ml-limits.h"
#include "mlstate-offsets.h"	/** this file is generated **/

/* The root registers in the ML state vector have the following layout,
 * where roots is guaranteed to be 8-byte aligned relative to the start
 * of the ML state vector (see "ml-state.h" and "mlstate-offsets.h"):
 *
 *              +----------------------+
 *     root --> | ml_stdlink (17)      |
 *              +----------------------+
 *      +4:     | ml_closure (18)      |
 *              +----------------------+
 *      +8:     | ml_arg (19)          |
 *              +----------------------+
 *      +12:    | ml_cont (20)         |
 *              +----------------------+
 *      +16:    | ml_exn (21)          |
 *              +----------------------+
 *      +20:    | ml_varptr (22)       |
 *              +----------------------+
 *      +24:    | ml_basereg (23)      |
 *              +----------------------+
 *      +28:    | unused               |
 *              +----------------------+
 *      +32:    | ml_pc		       |
 *              +----------------------+
 *      +36:    | misc 24-27,3-13      |
 *		.		       .
 *		.		       .
 *              +----------------------+
 */

/** register usage **/

#define		sp		1
#define 	stackptr	sp
#define		allocptr	14
#define 	limitptr	15
#define 	storeptr	16
#define		stdlink		17
#define 	stdclos		18
#define 	stdarg		19
#define 	stdcont		20
#define 	exncont		21
#define 	varptr		22
#define 	basereg		23
#define		miscreg0	24
#define		miscreg1	25
#define 	miscreg2	26
#define		miscreg3	27
#define		miscreg4	3
#define		miscreg5	4
#define		miscreg6	5
#define		miscreg7	6
#define		miscreg8	7
#define		miscreg9	8
#define		miscreg10	9
#define		miscreg11	10
#define		miscreg12	11
#define		miscreg13	12
#define		miscreg14	13
#define		gclink		28

#define       	atmp1 29
#define       	atmp2 30
#define       	atmp3 31
#define 	atmp4 miscreg13

/* stackframe layout:
 * Note: 1. The offset of cvti2d tmp is used in rs6000.sml
 *          float load/store offset is hardwired in rs6000instr.sml
 *       2. The offset of pseudo regs is used in rs6000/rs6000.sml 
 *          to support loadpseudo and storepseudo.
 *
 * 	             +-------------------+
 *   sp--> 0(sp)     | mlstate addr      |
 *	             +-------------------+
 *	   4(sp)     | _startgc addr	 |
 *	             +-------------------+
 *	   8(sp)     | cvti2d const	 |
 *	             +-------------------+
 *	  16(sp)     | cvti2d tmp2	 |
 *	      	     +-------------------+
 *	  24(sp)     | float load/store  |
 *		     +-------------------+
 *	  32(sp)     | floor tmp	 |
 *		     +-------------------+
 *	  40(sp)     | pseudo regs 1	 |
 *		     +-------------------+
 *	  44(sp)     | pseudo regs 2	 |
 *		     +-------------------+
 *  argblock(sp)     | C calleesave regs |
 *	             .		         .
 *		     .		         .
 *		     +-------------------+
 *  argblock+92(sp)  |		    	 |
 */


/** MLState offsets **/
#define		NROOTS			24	/* from ml_state.h */

#define argblock 		48
#define savearea		(23*4+4)	/* lr,cr,1,2,13-31,padding */
#define framesize		(argblock+savearea)
#define MLSTATE_OFFSET 		0	
#define STARTGC_OFFSET		4
#define CVTI2D_OFFSET		8
#define FLOOR_OFFSET		32
#define PSEUDOREG_OFFSET        40

/** offsets in condition register CR.0 **/

#define CR0_LT 0
#define CR0_GT 1
#define CR0_EQ 2
#define CR0_SO 3
	
#define CR0	0

/** special registers **/

#define SPR_LR	8

/** C parameter passing conventions **/
#define CARG1 		3
#define CRESULT1 	3


#if (CALLEESAVE > 0)
#define CONTINUE					\
	    cmpl	CR0,limitptr,allocptr;		\
	    mtlr	stdcont;			\
	    br
#endif

#define CHECKLIMIT(mask,label)	 			\
	    bbt		CR0_GT, label;			\
	    l		atmp2,STARTGC_OFFSET(stackptr);	\
	    lil		atmp1,mask;			\
	    mtspr	SPR_LR, atmp2;			\
	    ai		gclink,stdlink,0;		\
	    br		;				\
    label:

/** A comment explaining what this does would be nice!! **/
	.extern		CSYM(_PollFreq0{UA})
	.extern 	CSYM(_PollEvent0{UA})

	.toc
T._PollFreq0:
	.tc	H._PollFreq0{TC},_PollFreq0{UA}
T._PollEvent0:
	.tc	H._PollEvent0{TC},_PollEvent0{UA}

/* create table of contents entries. */
	.toc
	.csect 	prim.s[BS]
	.extern .saveregs

	.csect  [RO]
	.toc
cvti2d_CONST:	
	.tc 	fd43300000_80000000[TC],1127219200,-2147483648
T.saveregs:	
	.tc	saveregs[TC],.saveregs

	.csect	[PR]
/* sig_return : ('a cont * 'a) -> 'b
 */
ML_CODE_HDR(sigh_return_a)
	lil 	atmp1,RET_MASK
	lil 	atmp4,REQ_SIG_RETURN
	b	set_request

GLOBAL(sigh_resume)
sigh_resume:
	lil	atmp1,FUN_MASK
	lil	atmp4, REQ_SIG_RESUME
        b	set_request

/* pollh_return_a:
 * The return continuation for the ML poll handler.
 */
ML_CODE_HDR(pollh_return_a)
	lil	atmp1,RET_MASK
	lil	atmp4,REQ_POLL_RETURN
	b	set_request

/* pollh_resume:
 * Resume execution at the point at which a poll event occurred.
 */
ENTRY(pollh_resume)
	lil	atmp1,FUN_MASK
	lil	atmp4,REQ_POLL_RESUME
	b	set_request

		 /* exception handler for ML functions called from C */
ML_CODE_HDR(handle_a)
	lil	atmp1,EXN_MASK
	lil	atmp4,REQ_EXN
	b	set_request


		/* continuation for ML functions called from C */		
ML_CODE_HDR(return_a)
	lil	atmp1,RET_MASK
	lil	atmp4,REQ_RETURN
	b	set_request


GLOBAL(request_fault)

request_fault:
	lil	atmp1,EXN_MASK
	lil	atmp4,REQ_FAULT
	b	set_request


/* bind_cfun : (string * string) -> c_function
 */
ML_CODE_HDR(bind_cfun_a)
	CHECKLIMIT(FUN_MASK,bind_cfun_v_limit) 
	lil	atmp1,FUN_MASK
	lil	atmp4,REQ_BIND_CFUN
	b	set_request

ML_CODE_HDR(build_literals_a)
	CHECKLIMIT(FUN_MASK,build_literals_v_limit) 
	lil	atmp1,FUN_MASK
	lil	atmp4,REQ_BUILD_LITERALS
	b	set_request

ML_CODE_HDR(callc_a)
	CHECKLIMIT(FUN_MASK,callc_v_limit) 
	lil	atmp1,FUN_MASK
	lil	atmp4,REQ_CALLC
	/* fall through */

set_request:
	l	atmp3,MLSTATE_OFFSET(sp)	/* save the minimal ML state */
	st	atmp1,MaskOffMSP(atmp3)		/* mask */
	l	atmp2,VProcOffMSP(atmp3)	/* atmp2 := VProc State ptr */
	lil	0,0
	st  	0,InMLOffVSP(atmp2)		/* note that we have left ML */
	st	allocptr,AllocPtrOffMSP(atmp3)
	st	limitptr,LimitPtrOffMSP(atmp3)
	st	storeptr,StorePtrOffMSP(atmp3)
	st	stdlink,LinkRegOffMSP(atmp3)
	st	stdlink,PCOffMSP(atmp3)
	st	stdarg,StdArgOffMSP(atmp3)
	st	stdcont,StdContOffMSP(atmp3)
	st	stdclos,StdClosOffMSP(atmp3)
	st	varptr,VarPtrOffMSP(atmp3)
	st	exncont,ExnPtrOffMSP(atmp3)
#if CALLEESAVE > 0
	st	miscreg0,MiscRegOffMSP(0)(atmp3)
#if CALLEESAVE > 1
	st	miscreg1,MiscRegOffMSP(1)(atmp3)
#if CALLEESAVE > 2
	st	miscreg2,MiscRegOffMSP(2)(atmp3)
#if CALLEESAVE > 3
	??
#endif
#endif
#endif
#endif
	ai	3,atmp4,0			/* request as argument */

restore_c_regs:
        l       atmp1,PSEUDOREG_OFFSET(sp)     /* restore pseudo registers */
        st      atmp1,PseudoReg1OffMSP(atmp3)   
        l       atmp1,PSEUDOREG_OFFSET+4(sp)
        st      atmp1,PseudoReg2OffMSP(atmp3)
 	l	2, (argblock+4)(sp) 
	l	13, (argblock+8)(sp)
	l	14, (argblock+12)(sp)
	l 	15, (argblock+16)(sp)
	l	16, (argblock+20)(sp)
	l	17, (argblock+24)(sp)
	l	18, (argblock+28)(sp)
	l 	19, (argblock+32)(sp)
	l 	20, (argblock+36)(sp)
	l	21, (argblock+40)(sp)
	l 	22, (argblock+44)(sp)
	l 	23, (argblock+48)(sp)
	l 	24, (argblock+52)(sp)
	l	25, (argblock+56)(sp)
	l 	26, (argblock+60)(sp)
	l	27, (argblock+64)(sp)
	l	28, (argblock+68)(sp)
	l 	29, (argblock+72)(sp)
	l	30, (argblock+76)(sp)
	l 	31, (argblock+80)(sp)
	l	0, (argblock+84)(sp)
	mtlr    0
	l	0, (argblock+88)(sp)
	mtcrf	0x80, 0
	ai	sp,sp,framesize 
	br

GLOBAL(.saveregs)
	.csect [PR]
.saveregs:
	l	atmp3,MLSTATE_OFFSET(sp)
	st	atmp1,MaskOffMSP(atmp3)

#ifdef SOFT_POLL
	/* free some regs */
	st	miscreg0,MiscRegOffMSP(0)(atmp3)	/* use as tmp */
	st	miscreg1,MiscRegOffMSP(1)(atmp3)	/* use as tmp */
#define pfreq	atmp1
#define	pevent	miscreg0
#define ptmp    miscreg1

	/* check if polling enabled (PollFreq > 0) */
	l	pfreq,T._PollFreq0(2)		/* address of PollFreq */
	l	pfreq,4(pfreq)			/* contents of PollFreq */
	sri	pfreq,pfreq,1			/* strip integer tag */
	cmpi	CR0,pfreq,0
	bbt	CR0_EQ,check_for_gc		/* go check for real gc */
	l	ptmp,InPollHandlerOffMSP(atmp3)	/* if we're in the handler */
	cmpi	CR0,ptmp,0
	bbt	CR0_GT,reset_limit		/* ignore poll events */
	l	ptmp,T._PollEvent0(2)		/* address of PollEvent */
	l	pevent,4(ptmp)			/* contents of PollEvent */
	sri	pevent,pevent,1
	cmpi	CR0,pevent,0
	bbt	CR0_EQ,reset_limit
	/* event occurred, so set ml_pollHandlerPending */
	lil	ptmp,1
	st	ptmp,PollPendingOffMSP(atmp3)
	b	do_gc		/* and handle event in the C runtime */

reset_limit:				/* reset limit ptr */
	sli	pfreq,pfreq,POLL_GRAIN_BITS	/* mult by POLL_GRAIN_CPSI */
	a	limitptr,pfreq,allocptr

check_for_gc:
#define	ptmp2	pfreq
#define vsp	pevent
	/* ensure real limit is >= limit */
	l	ptmp,RealLimitOffMSP(atmp3)
	cmpl	CR0,ptmp,limitptr
	bbt	CR0_GT,ok_limit
	ai	limitptr,ptmp,0           /* move ptmp into limit */
ok_limit:
	ai	ptmp,limitptr,-4096
	cmpl	CR0,ptmp,allocptr
	bbf	CR0_GT,do_gc 		/* gc *//* should be a common case */
	/* since a signal also sets limitptr == allocptr to force a trap, */
	/* we need to disambiguate poll-events/signals here */
	l	vsp,VProcOffMSP(atmp3)          /* get the vsp */
	l	ptmp,PollPendingOffMSP(atmp3)
	l	ptmp2,NPendingOffVSP(vsp)
	a	ptmp,ptmp,ptmp2
	l	ptmp2,NPendingSysOffVSP(vsp)
	a	ptmp,ptmp,ptmp2
	cmpi	CR0,ptmp,0
	bbt	CR0_GT,do_gc
#undef  vsp
#undef  ptmp2

no_gc:	/* an uneventful poll check, back to ML */
	l	miscreg0,MiscRegOffMSP(0)(atmp3)  /* reload miscregs */
	l	miscreg1,MiscRegOffMSP(1)(atmp3)
	b 	ml_go

do_gc:
	st	limitptr,LimitPtrOffMSP(atmp3)

#undef pfreq
#undef pevent
#undef ptmp
#endif /* SOFT_POLL */

	l	atmp2,VProcOffMSP(atmp3)	/* atmp2 := VProc State ptr */
	lil	0,0
	st	0,InMLOffVSP(atmp2)		/* note that we have left ML */
	ai	basereg, basereg, -32764
	st	allocptr,AllocPtrOffMSP(atmp3)
	st	storeptr,StorePtrOffMSP(atmp3)
	st	stdarg,StdArgOffMSP(atmp3)
	st	stdcont,StdContOffMSP(atmp3)
	st	stdclos,StdClosOffMSP(atmp3)
	st	gclink,PCOffMSP(atmp3)
	st	exncont,ExnPtrOffMSP(atmp3)
	/* save misc. roots */	
#ifndef SOFT_POLL   /* miscreg0 & miscreg1 saved above for SOFT_POLL */
	st	miscreg0,MiscRegOffMSP(0)(atmp3)	
	st	miscreg1,MiscRegOffMSP(1)(atmp3)
#endif
	st	miscreg2,MiscRegOffMSP(2)(atmp3)
	st	miscreg3,MiscRegOffMSP(3)(atmp3)
	st	miscreg4,MiscRegOffMSP(4)(atmp3)
	st	miscreg5,MiscRegOffMSP(5)(atmp3)
	st	miscreg6,MiscRegOffMSP(6)(atmp3)
	st	miscreg7,MiscRegOffMSP(7)(atmp3)
	st	miscreg8,MiscRegOffMSP(8)(atmp3)
	st	miscreg9,MiscRegOffMSP(9)(atmp3)
	st	miscreg10,MiscRegOffMSP(10)(atmp3)
	st	miscreg11,MiscRegOffMSP(11)(atmp3)
	st	miscreg12,MiscRegOffMSP(12)(atmp3)
	st	miscreg13,MiscRegOffMSP(13)(atmp3)
	st	miscreg14,MiscRegOffMSP(14)(atmp3)
	st	stdlink,LinkRegOffMSP(atmp3)
	st	basereg,BasePtrOffMSP(atmp3)		/* base reg */
	st	varptr,VarPtrOffMSP(atmp3)
        lil	CARG1,REQ_GC
	b	restore_c_regs


GLOBAL(.restoreregs)
.restoreregs:
	ai	sp,sp,-framesize
	l	0,T.saveregs(2)
	st	3, MLSTATE_OFFSET(sp)
	st	0, STARTGC_OFFSET(sp)
	lfd	0, cvti2d_CONST(2)
	stfd	0, CVTI2D_OFFSET(sp)

	st	2, argblock+4(sp)
	st	13, argblock+8(sp)
	st	14, argblock+12(sp)
	st 	15, argblock+16(sp)
	st	16, argblock+20(sp)
	st	17, argblock+24(sp)
	st	18, argblock+28(sp)
	st 	19, argblock+32(sp)
	st 	20, argblock+36(sp)
	st	21, argblock+40(sp)
	st 	22, argblock+44(sp)
	st 	23, argblock+48(sp)
	st 	24, argblock+52(sp)
	st	25, argblock+56(sp)
	st 	26, argblock+60(sp)
	st	27, argblock+64(sp)
	st	28, argblock+68(sp)
	st 	29, argblock+72(sp)
	st	30, argblock+76(sp)
	st 	31, argblock+80(sp)
	mflr    0
	st	0,  argblock+84(sp)
	mfcr	0
	st	0,  argblock+88(sp)
	
	and	atmp1,3,3			/* atmp1 := MLState pointer */

	l	allocptr,AllocPtrOffMSP(atmp1)
	l	limitptr,LimitPtrOffMSP(atmp1)
	l	storeptr,StorePtrOffMSP(atmp1)
        l       atmp2,PseudoReg1OffMSP(atmp1)   /* restore pseudo registers */
        l       atmp3,PseudoReg2OffMSP(atmp1)
        st      atmp2,PSEUDOREG_OFFSET(sp)
        st      atmp3,PSEUDOREG_OFFSET+4(sp)
	l 	atmp2,VProcOffMSP(atmp1)	/* atmp2 := VProc State ptr */
	lil	atmp3,1
	st	atmp3,InMLOffVSP(atmp2)         /* we are entering ML code */
	l	stdarg,StdArgOffMSP(atmp1)
	l	stdcont,StdContOffMSP(atmp1)
	l	stdclos,StdClosOffMSP(atmp1)
	l	exncont,ExnPtrOffMSP(atmp1)
	l	miscreg0,MiscRegOffMSP(0)(atmp1)
	l	miscreg1,MiscRegOffMSP(1)(atmp1)
	l	miscreg2,MiscRegOffMSP(2)(atmp1)
	l	miscreg3,MiscRegOffMSP(3)(atmp1)
	l	miscreg4,MiscRegOffMSP(4)(atmp1)
	l	miscreg5,MiscRegOffMSP(5)(atmp1)
	l	miscreg6,MiscRegOffMSP(6)(atmp1)
	l	miscreg7,MiscRegOffMSP(7)(atmp1)
	l	miscreg8,MiscRegOffMSP(8)(atmp1)
	l	miscreg9,MiscRegOffMSP(9)(atmp1)
	l	miscreg10,MiscRegOffMSP(10)(atmp1)
	l	miscreg11,MiscRegOffMSP(11)(atmp1)
	l	miscreg12,MiscRegOffMSP(12)(atmp1)
	l	miscreg13,MiscRegOffMSP(13)(atmp1)
	l	miscreg14,MiscRegOffMSP(14)(atmp1)
	l	stdlink,LinkRegOffMSP(atmp1)
	l	varptr,VarPtrOffMSP(atmp1)
	l 	basereg,BasePtrOffMSP(atmp1)
	l	gclink,PCOffMSP(atmp1)
	ai      basereg,basereg,32764		/* adjust baseReg */
						/* check for pending signals */
	l	atmp1,NPendingSysOffVSP(atmp2)
	l	atmp3,NPendingOffVSP(atmp2)
	a	atmp1,atmp1,atmp3
	cmpi	CR0,atmp1,0
	bbf	CR0_EQ,pending_sigs


GLOBAL(ml_go) 
ml_go:	
	cmpl	CR0,limitptr,allocptr
	mtlr	gclink

	mtfsfi  3,0			/* Ensure that no exceptions are set */
	mtfsfi  2,0
	mtfsfi  1,0
	mtfsfi  0,0
	lil	0,0
	mtxer   0
	br				/* jump to ML code */

pending_sigs:				/* there are pending signals */
	l	atmp1,InSigHandlerOffVSP(atmp2)
	cmpi	CR0,atmp1,0
	bbf 	CR0_EQ,ml_go
					/* check if currently handling a signal */	
	l	atmp1,InSigHandlerOffVSP(atmp2)
	cmpi	CR0,atmp1,0
	bbf	CR0_EQ,ml_go

	lil	0,1
	st	0,HandlerPendingOffVSP(atmp2)
	ai	limitptr,allocptr,0
	b	ml_go


GLOBAL(.SaveFPRegs)
	.align 2

ML_CODE_HDR(array_a)
	CHECKLIMIT(FUN_MASK,array_a_limit)
	l	atmp2,0(stdarg)		/* atmp2 := tagged length */
	srai	atmp2,atmp2,1		/* atmp2 := untagged length */
 	cmpi	CR0,atmp2,SMALL_OBJ_SZW /* is this a large object */
	bbf	CR0_LT,array_a_large	
	sli	atmp3,atmp2,TAG_SHIFTW  /* atmp3 := build descriptor */
	oril	atmp3,atmp3,MAKE_TAG(DTAG_array)
	st	atmp3,0(allocptr)	/* store the descriptor */
	ai	allocptr,allocptr,4	/* points to new object */
	l	atmp4,4(stdarg)		/* atmp4 := get initial value */
	ai	stdarg,allocptr,0	/* put ptr in return register */
	sli	atmp2,atmp2,2		/* atmp2 := length in bytes */
	a	atmp1,atmp2,allocptr	/* beyond last word of new array */
array_a_2:				/* loop */
        ai	allocptr,allocptr,4	/* on to the next word */
	cmp	CR0,allocptr,atmp1
	st	atmp4,-4(allocptr)	/* store the value */
	bbf	CR0_EQ, array_a_2	/* if not off the end, repeat */
					/* end loop */
	CONTINUE
array_a_large:				/* off-line allocation */
	lil	atmp1,FUN_MASK
	lil	atmp4,REQ_ALLOC_ARRAY
	b	set_request


ML_CODE_HDR(create_b_a)
	CHECKLIMIT(FUN_MASK,create_b_a_limit)
	srai	atmp2,stdarg,1		/* atmp1 = length */
	ai	atmp2,atmp2,3
	srai	atmp2,atmp2,2		/* length in words (including desc) */
	cmpi    CR0,atmp2,SMALL_OBJ_SZW /* is this a small object */
	bbf     CR0_LT,create_b_a_large

	srai	atmp3,stdarg,1		/* build descriptor in atmp3 */
	sli	atmp3,atmp3,TAG_SHIFTW
	oril	atmp3,atmp3,MAKE_TAG(DTAG_bytearray)
	st	atmp3,0(allocptr)	/* store descriptor */
	ai	stdarg,allocptr,4	/* pointer to new string */
	sli	atmp2,atmp2,2		/* length in bytes */
	ai	atmp2,atmp2,4		/* length + tag */
	a	allocptr,allocptr,atmp2	/* advance allocptr */
	CONTINUE

create_b_a_large:
	lil	atmp1,FUN_MASK
	lil 	atmp4,REQ_ALLOC_BYTEARRAY
	b	set_request	


/*
** create_s_a: int -> string
*/
ML_CODE_HDR(create_s_a)
	CHECKLIMIT(FUN_MASK,create_s_a_limit)
	srai	atmp2,stdarg,1		/* atmp1 = length */
	ai	atmp2,atmp2,4
	srai	atmp2,atmp2,2		/* length in words (including desc) */
	cmpi	CR0,atmp2,SMALL_OBJ_SZW /* is this a small object */
	bbf	CR0_LT,create_s_a_large
	
	srai	atmp1,stdarg,1		/* build descriptor in atmp1 */
	sli	atmp1,atmp1,TAG_SHIFTW
	oril	atmp1,atmp1,MAKE_TAG(DTAG_string)
	st	atmp1,0(allocptr)	/* store descriptor */
	ai	stdarg,allocptr,4	/* pointer to new string */
	sli	atmp2,atmp2,2		/* length in bytes */
	ai	atmp2,atmp2,4		/* + tag */
	a	allocptr,allocptr,atmp2	/* advance allocptr */
	lil	0,0
	st	0,-4(allocptr)		/* zero in last word */
	CONTINUE

create_s_a_large:
	lil	atmp1,FUN_MASK
	lil	atmp4,REQ_ALLOC_STRING
	b	set_request



ML_CODE_HDR(create_r_a)
	CHECKLIMIT(FUN_MASK,create_r_a_limit)
	srai	atmp2,stdarg,1		/* atmp1 = length */
	sli	atmp2,atmp2,1		/* length in words */
	cmpi	CR0,atmp2,SMALL_OBJ_SZW	/* is this a small object */
	bbf	CR0_LT,create_r_a_large
	
	srai	atmp3,stdarg,1		/* descriptor in atmp3 */
	sli	atmp3,atmp3,TAG_SHIFTW
	oril	atmp3,atmp3,MAKE_TAG(DTAG_realdarray)
#ifdef ALIGN_REALDS
	oril	allocptr,allocptr,4
#endif	
	st	atmp3,0(allocptr)
	ai	stdarg,allocptr,4	/* pointer to new realarray */
	sli	atmp2,atmp2,2		/* length in bytes */
	ai	atmp2,atmp2,4		/* plus tag */
	a	allocptr,allocptr,atmp2	/* new allocptr */
	CONTINUE
create_r_a_large:
	lil	atmp1,FUN_MASK
	lil	atmp4,REQ_ALLOC_REALDARRAY
	b	set_request


ML_CODE_HDR(create_v_a)
	CHECKLIMIT(FUN_MASK,create_v_a_limit)
	l	atmp1,0(stdarg)		/* atmp1 := tagged length */
	srai	atmp1,atmp1,1		/* untagged length */
	cmpi	CR0,atmp1,SMALL_OBJ_SZW /* is this a small object */
	bbf	CR0_LT,create_v_a_large

	sli	atmp2,atmp1,TAG_SHIFTW	/* build descriptor in atmp2 */
	oril	atmp2,atmp2,MAKE_TAG(DTAG_vector)
	st	atmp2,0(allocptr)
	ai	allocptr,allocptr,4
	l	atmp2,4(stdarg)		/* atmp2 := list */
	ai	stdarg,allocptr,0	/* stdarg := vector */
	lil	atmp3,ML_nil

create_v_a_1:
	l	atmp1,0(atmp2)		/* atmp1:=hd(atmp2) */
	l	atmp2,4(atmp2)		/* atmp2:=tl(atmp2) */
	cmp	CR0,atmp2,atmp3
	st	atmp1,0(allocptr)	/* store word */
	ai	allocptr,allocptr,4
	bbf	CR0_EQ,create_v_a_1

	CONTINUE

create_v_a_large:
	lil	atmp1,FUN_MASK
	lil	atmp4,REQ_ALLOC_VECTOR
	b	set_request


	.csect	[RO]
	.toc
floor_CONST:	.tc	fd43300800_0[TC],0x43300800, 0

	.csect 	[PR]
	/*
	** floor_a : real -> int
        **  Caller must ensure arg in range.
	**	This code essentially loads 1.0*2^52 into 
	**	register f3. A floating add will internally 
	**	perform an exponent alignment, which will 
	**	bring the required bits into the mantissa.
	*/
ML_CODE_HDR(floor_a)
	lfd	1, 0(stdarg)		
	/*
	** Neat thing here is that this code works for
	** both +ve and -ve floating point numbers.
	*/
	mffs	0
	stfd	0,0(allocptr)	/* steal the allocptr for a second */
	l 	0, 4(allocptr)
	mtfsb1	30
	mtfsb1 	31
	lfd	3,floor_CONST(2)	
	fa	6,1,3
	stfd	6,FLOOR_OFFSET(sp)
	l	stdarg,FLOOR_OFFSET+4(sp)
	a	stdarg,stdarg,stdarg
	ai	stdarg,stdarg,1
	
	andil.	0,0, 0xf
	mtfsf	0xff,0
	CONTINUE



ML_CODE_HDR(logb_a)
	l 	stdarg,0(stdarg)  	/* most significant part */
	srai 	stdarg,stdarg,20	/* throw out 20 low bits */
	andil.	stdarg,stdarg,0x07ff	/* clear all but 11 low bits */
	ai	stdarg,stdarg,-1023	/* subtract 1023 */
	sli	stdarg,stdarg,1		/* make room for tag bit */
	ai	stdarg,stdarg,1		/* add the tag bit */
	CONTINUE


/*
** scalb : real * int -> real
**	scalb(x,y) = x * 2^y
*/
ML_CODE_HDR(scalb_a)
	CHECKLIMIT(FUN_MASK,scalb_v_limit)
	l	atmp1,4(stdarg)		/* atmp1 := y */
	srai	atmp1,atmp1,1		/* atmp1 := machine int y */
	l	stdarg,0(stdarg)	/* stdarg := x */
	l	atmp2,0(stdarg)		/* atmp2 := MSW(x) */
	liu	0,0x7ff0		/* r0 := 0x7ff0,0000 */
	and.	atmp3,atmp2,0		/* atmp3 := atmp2 & 0x7ff00000 */
	bbt	CR0_EQ,scalb_all_done
	
	srai	atmp3,atmp3,20		/* atmp3 := ieee(exp) */
	a.	atmp1,atmp1,atmp3	/* scale exponent */
	bbt	CR0_LT,scalb_underflow

	cmpi	CR0,atmp1,2047		/* max. ieee(exp) */
	bbf	CR0_LT,scalb_overflow

	sfi	0,0,-1			/* r0 := not(r0) */
	and	atmp2,atmp2,0		/* atmp2 := high mantessa bits + sign */
	sli	atmp1,atmp1,20		/* atmp1 := new exponent */
	or	atmp1,atmp1,atmp2	/* atmp1 := new MSB(x) */
	l 	atmp2, 4(stdarg)	

scalb_write_out:
	st	atmp1, 4(allocptr)
	st	atmp2, 8(allocptr)
	lil	atmp3, DESC_reald
	st	atmp3, 0(allocptr)
	ai	stdarg,allocptr,4
	ai	allocptr,allocptr,12

scalb_all_done:
	CONTINUE

scalb_underflow:
	lil	atmp1,0
	lil	atmp2,0
	b	scalb_write_out

LABEL(scalb_overflow)
	mtfsb1 	3



ML_CODE_HDR(try_lock_a)
	l	atmp1,0(stdarg)
	lil	atmp2,1			/* ML_false */
	st	atmp2,0(stdarg)
	ai	stdarg,atmp1,0
	CONTINUE


ML_CODE_HDR(unlock_a)
	lil	atmp1,3			/* ML_true */
	st	atmp1,0(stdarg)
	lil	stdarg,1		/* just return unit */
	CONTINUE



/* saveFPRegs and restoreFPRegs are called from C only. */
#define ctmp1 12
#define ctmp2 11
#define ctmp3 10


LABEL(.SaveFPRegs)
	stfd	14, 4(3)
	stfd	15, 12(3)
	stfd	16, 20(3)
	stfd	17, 28(3)
	stfd	18, 36(3)
	stfd	19, 44(3)
	stfd	20, 52(3)
	stfd	21, 60(3)
	stfd	22, 68(3)
	stfd	23, 76(3)
	stfd	24, 84(3)
	stfd	25, 92(3)
	stfd	26, 100(3)
	stfd	27, 108(3)
	stfd	28, 116(3)
	stfd	29, 124(3)
	stfd	30, 132(3)
	stfd	31, 140(3)

	br

GLOBAL(.RestoreFPRegs)
	.align 2
LABEL(.RestoreFPRegs)
	lfd	14, 0(3)
	lfd	15, 8(3)
	lfd	16, 16(3)
	lfd	17, 24(3)
	lfd	18, 32(3)
	lfd	19, 40(3)
	lfd	20, 48(3)
	lfd	21, 56(3)
	lfd	22, 64(3)
	lfd	23, 72(3)
	lfd	24, 80(3)
	lfd	25, 88(3)
	lfd	26, 96(3)
	lfd	27, 104(3)
	lfd	28, 112(3)
	lfd	29, 120(3)
	lfd	30, 128(3)
	lfd	31, 136(3)
	br

