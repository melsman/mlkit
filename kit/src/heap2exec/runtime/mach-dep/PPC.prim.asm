/* PPC.prim.asm
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
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
	    blr
#endif

#define CHECKLIMIT(mask,label)	 			\
	    bt		CR0_GT, label;			\
	    lwz		atmp2,STARTGC_OFFSET(stackptr);	\
	    li		atmp1,mask;			\
	    mtspr	SPR_LR, atmp2;			\
	    addi	gclink,stdlink,0;		\
	    blr		;				\
    label:

	.extern		CSYM(_PollFreq0)
	.extern 	CSYM(_PollEvent0)
	.extern		CSYM(saveregs)

#if defined (USE_TOC)
/* create table of contents entries for things we need the address of. */
	.toc
T._PollFreq0:
	.tc	H._PollFreq0[TC],CSYM(_PollFreq0)
T._PollEvent0:
	.tc	H._PollEvent0[TC],CSYM(_PollEvent0)
T.saveregs:	
	.tc	H.saveregs[TC],CSYM(saveregs)
T.cvti2d_CONST:
	.tc	H.cvti2d_CONST[TC],cvti2d_CONST
#endif
	RO_DATA
	ALIGN8
cvti2d_CONST:	
	DOUBLE(4503601774854144.0)

	TEXT
/* sig_return : ('a cont * 'a) -> 'b
 */
ML_CODE_HDR(sigh_return_a)
	li 	atmp1,RET_MASK
	li 	atmp4,REQ_SIG_RETURN
	b	set_request

ENTRY(sigh_resume)
	li	atmp1,FUN_MASK
	li	atmp4, REQ_SIG_RESUME
        b	set_request

/* pollh_return_a:
 * The return continuation for the ML poll handler.
 */
ML_CODE_HDR(pollh_return_a)
	li	atmp1,RET_MASK
	li	atmp4,REQ_POLL_RETURN
	b	set_request

/* pollh_resume:
 * Resume execution at the point at which a poll event occurred.
 */
ENTRY(pollh_resume)
	li	atmp1,FUN_MASK
	li	atmp4,REQ_POLL_RESUME
	b	set_request

		 /* exception handler for ML functions called from C */
ML_CODE_HDR(handle_a)
	li	atmp1,EXN_MASK
	li	atmp4,REQ_EXN
	b	set_request


		/* continuation for ML functions called from C */		
ML_CODE_HDR(return_a)
	li	atmp1,RET_MASK
	li	atmp4,REQ_RETURN
	b	set_request


ENTRY(request_fault)
	li	atmp1,EXN_MASK
	li	atmp4,REQ_FAULT
	b	set_request


/* bind_cfun : (string * string) -> c_function
 */
ML_CODE_HDR(bind_cfun_a)
	CHECKLIMIT(FUN_MASK,bind_cfun_v_limit) 
	li	atmp1,FUN_MASK
	li	atmp4,REQ_BIND_CFUN
	b	set_request

ML_CODE_HDR(build_literals_a)
	CHECKLIMIT(FUN_MASK,build_literals_v_limit) 
	li	atmp1,FUN_MASK
	li	atmp4,REQ_BUILD_LITERALS
	b	set_request

ML_CODE_HDR(callc_a)
	CHECKLIMIT(FUN_MASK,callc_v_limit) 
	li	atmp1,FUN_MASK
	li	atmp4,REQ_CALLC
	/* fall through */

set_request:
	lwz	atmp3,MLSTATE_OFFSET(sp)	/* save the minimal ML state */
	stw	atmp1,MaskOffMSP(atmp3)		/* mask */
	lwz	atmp2,VProcOffMSP(atmp3)	/* atmp2 := VProc State ptr */
	li	0,0
	stw	0,InMLOffVSP(atmp2)		/* note that we have left ML */
	stw	allocptr,AllocPtrOffMSP(atmp3)
	stw	limitptr,LimitPtrOffMSP(atmp3)
	stw	storeptr,StorePtrOffMSP(atmp3)
	stw	stdlink,LinkRegOffMSP(atmp3)
	stw	stdlink,PCOffMSP(atmp3)
	stw	stdarg,StdArgOffMSP(atmp3)
	stw	stdcont,StdContOffMSP(atmp3)
	stw	stdclos,StdClosOffMSP(atmp3)
	stw	varptr,VarPtrOffMSP(atmp3)
	stw	exncont,ExnPtrOffMSP(atmp3)
#if CALLEESAVE > 0
	stw	miscreg0,MiscRegOffMSP(0)(atmp3)
#if CALLEESAVE > 1
	stw	miscreg1,MiscRegOffMSP(1)(atmp3)
#if CALLEESAVE > 2
	stw	miscreg2,MiscRegOffMSP(2)(atmp3)
#if CALLEESAVE > 3
	??
#endif
#endif
#endif
#endif
	addi	3,atmp4,0			/* request as argument */

restore_c_regs:
        lwz	atmp1,PSEUDOREG_OFFSET(sp)     /* restore pseudo registers */
        stw	atmp1,PseudoReg1OffMSP(atmp3)   
        lwz	atmp1,PSEUDOREG_OFFSET+4(sp)
        stw	atmp1,PseudoReg2OffMSP(atmp3)
 	lwz	2, (argblock+4)(sp) 
	lwz	13, (argblock+8)(sp)
	lwz	14, (argblock+12)(sp)
	lwz	15, (argblock+16)(sp)
	lwz	16, (argblock+20)(sp)
	lwz	17, (argblock+24)(sp)
	lwz	18, (argblock+28)(sp)
	lwz	19, (argblock+32)(sp)
	lwz	20, (argblock+36)(sp)
	lwz	21, (argblock+40)(sp)
	lwz	22, (argblock+44)(sp)
	lwz	23, (argblock+48)(sp)
	lwz	24, (argblock+52)(sp)
	lwz	25, (argblock+56)(sp)
	lwz	26, (argblock+60)(sp)
	lwz	27, (argblock+64)(sp)
	lwz	28, (argblock+68)(sp)
	lwz	29, (argblock+72)(sp)
	lwz	30, (argblock+76)(sp)
	lwz	31, (argblock+80)(sp)
	lwz	0, (argblock+84)(sp)
	mtlr    0
	lwz	0, (argblock+88)(sp)
	mtcrf	0x80, 0
	addi	sp,sp,framesize 
	blr

	TEXT
ENTRY(saveregs)
	lwz	atmp3,MLSTATE_OFFSET(sp)
	stw	atmp1,MaskOffMSP(atmp3)

#ifdef SOFT_POLL
	/* free some regs */
	stw	miscreg0,MiscRegOffMSP(0)(atmp3)	/* use as tmp */
	stw	miscreg1,MiscRegOffMSP(1)(atmp3)	/* use as tmp */
#define pfreq	atmp1
#define	pevent	miscreg0
#define ptmp    miscreg1

	/* check if polling enabled (PollFreq > 0) */
	lwz	pfreq,T._PollFreq0(2)		/* address of PollFreq */
	lwz	pfreq,4(pfreq)			/* contents of PollFreq */
	sri	pfreq,pfreq,1			/* strip integer tag */
	cmpi	CR0,pfreq,0
	bt	CR0_EQ,check_for_gc		/* go check for real gc */
	lwz	ptmp,InPollHandlerOffMSP(atmp3)	/* if we're in the handler */
	cmpi	CR0,ptmp,0
	bt	CR0_GT,reset_limit		/* ignore poll events */
	lwz	ptmp,T._PollEvent0(2)		/* address of PollEvent */
	lwz	pevent,4(ptmp)			/* contents of PollEvent */
	sri	pevent,pevent,1
	cmpi	CR0,pevent,0
	bt	CR0_EQ,reset_limit
	/* event occurred, so set ml_pollHandlerPending */
	li	ptmp,1
	stw	ptmp,PollPendingOffMSP(atmp3)
	b	do_gc		/* and handle event in the C runtime */

reset_limit:				/* reset limit ptr */
	slwi	pfreq,pfreq,POLL_GRAIN_BITS	/* mult by POLL_GRAIN_CPSI */
	add	limitptr,pfreq,allocptr

check_for_gc:
#define	ptmp2	pfreq
#define vsp	pevent
	/* ensure real limit is >= limit */
	lwz	ptmp,RealLimitOffMSP(atmp3)
	cmpl	CR0,ptmp,limitptr
	bt	CR0_GT,ok_limit
	addi	limitptr,ptmp,0           /* move ptmp into limit */
ok_limit:
	addi	ptmp,limitptr,-4096
	cmpl	CR0,ptmp,allocptr
	bf	CR0_GT,do_gc 		/* gc *//* should be a common case */
	/* since a signal also sets limitptr == allocptr to force a trap, */
	/* we need to disambiguate poll-events/signals here */
	lwz	vsp,VProcOffMSP(atmp3)          /* get the vsp */
	lwz	ptmp,PollPendingOffMSP(atmp3)
	lwz	ptmp2,NPendingOffVSP(vsp)
	add	ptmp,ptmp,ptmp2
	lwz	ptmp2,NPendingSysOffVSP(vsp)
	add	ptmp,ptmp,ptmp2
	cmpi	CR0,ptmp,0
	bt	CR0_GT,do_gc
#undef  vsp
#undef  ptmp2

no_gc:	/* an uneventful poll check, back to ML */
	lwz	miscreg0,MiscRegOffMSP(0)(atmp3)  /* reload miscregs */
	lwz	miscreg1,MiscRegOffMSP(1)(atmp3)
	b 	CSYM(ml_go)

do_gc:
	stw	limitptr,LimitPtrOffMSP(atmp3)

#undef pfreq
#undef pevent
#undef ptmp
#endif /* SOFT_POLL */

	lwz	atmp2,VProcOffMSP(atmp3)	/* atmp2 := VProc State ptr */
	li	0,0
	stw	0,InMLOffVSP(atmp2)		/* note that we have left ML */
	addi	basereg, basereg, -32764
	stw	allocptr,AllocPtrOffMSP(atmp3)
	stw	storeptr,StorePtrOffMSP(atmp3)
	stw	stdarg,StdArgOffMSP(atmp3)
	stw	stdcont,StdContOffMSP(atmp3)
	stw	stdclos,StdClosOffMSP(atmp3)
	stw	gclink,PCOffMSP(atmp3)
	stw	exncont,ExnPtrOffMSP(atmp3)
	/* save misc. roots */	
#ifndef SOFT_POLL   /* miscreg0 & miscreg1 saved above for SOFT_POLL */
	stw	miscreg0,MiscRegOffMSP(0)(atmp3)	
	stw	miscreg1,MiscRegOffMSP(1)(atmp3)
#endif
	stw	miscreg2,MiscRegOffMSP(2)(atmp3)
	stw	miscreg3,MiscRegOffMSP(3)(atmp3)
	stw	miscreg4,MiscRegOffMSP(4)(atmp3)
	stw	miscreg5,MiscRegOffMSP(5)(atmp3)
	stw	miscreg6,MiscRegOffMSP(6)(atmp3)
	stw	miscreg7,MiscRegOffMSP(7)(atmp3)
	stw	miscreg8,MiscRegOffMSP(8)(atmp3)
	stw	miscreg9,MiscRegOffMSP(9)(atmp3)
	stw	miscreg10,MiscRegOffMSP(10)(atmp3)
	stw	miscreg11,MiscRegOffMSP(11)(atmp3)
	stw	miscreg12,MiscRegOffMSP(12)(atmp3)
	stw	miscreg13,MiscRegOffMSP(13)(atmp3)
	stw	miscreg14,MiscRegOffMSP(14)(atmp3)
	stw	stdlink,LinkRegOffMSP(atmp3)
	stw	basereg,BasePtrOffMSP(atmp3)		/* base reg */
	stw	varptr,VarPtrOffMSP(atmp3)
        li	CARG1,REQ_GC
	b	restore_c_regs


CENTRY(restoreregs)
	addi	sp,sp,-framesize
#if defined(USE_TOC)
	lwz	0,T.saveregs(2)
#else
	lis	0, CSYM(saveregs)@ha		/* GPR0 <- addrof(saveregs) */
	addi	0, 0, CSYM(saveregs)@l
#endif
	stw	3, MLSTATE_OFFSET(sp)
	stw	0, STARTGC_OFFSET(sp)
#if defined(USE_TOC)
	lwz	4, T.cvti2d_CONST(2)		/* GPR2 is RTOC */
	lfd	0, 0(4)
#else
	lis	4, cvti2d_CONST@ha
	lfd	0, cvti2d_CONST@l(4)
#endif
	stfd	0, CVTI2D_OFFSET(sp)

	stw	2, argblock+4(sp)
	stw	13, argblock+8(sp)
	stw	14, argblock+12(sp)
	stw	15, argblock+16(sp)
	stw	16, argblock+20(sp)
	stw	17, argblock+24(sp)
	stw	18, argblock+28(sp)
	stw	19, argblock+32(sp)
	stw	20, argblock+36(sp)
	stw	21, argblock+40(sp)
	stw	22, argblock+44(sp)
	stw	23, argblock+48(sp)
	stw	24, argblock+52(sp)
	stw	25, argblock+56(sp)
	stw	26, argblock+60(sp)
	stw	27, argblock+64(sp)
	stw	28, argblock+68(sp)
	stw	29, argblock+72(sp)
	stw	30, argblock+76(sp)
	stw	31, argblock+80(sp)
	mflr    0
	stw	0,  argblock+84(sp)
	mfcr	0
	stw	0,  argblock+88(sp)
	
	and	atmp1,3,3			/* atmp1 := MLState pointer */

	lwz	allocptr,AllocPtrOffMSP(atmp1)
	lwz	limitptr,LimitPtrOffMSP(atmp1)
	lwz	storeptr,StorePtrOffMSP(atmp1)
        lwz	atmp2,PseudoReg1OffMSP(atmp1)   /* restore pseudo registers */
        lwz	atmp3,PseudoReg2OffMSP(atmp1)
        stw	atmp2,PSEUDOREG_OFFSET(sp)
        stw	atmp3,PSEUDOREG_OFFSET+4(sp)
	lwz	atmp2,VProcOffMSP(atmp1)	/* atmp2 := VProc State ptr */
	li	atmp3,1
	stw	atmp3,InMLOffVSP(atmp2)         /* we are entering ML code */
	lwz	stdarg,StdArgOffMSP(atmp1)
	lwz	stdcont,StdContOffMSP(atmp1)
	lwz	stdclos,StdClosOffMSP(atmp1)
	lwz	exncont,ExnPtrOffMSP(atmp1)
	lwz	miscreg0,MiscRegOffMSP(0)(atmp1)
	lwz	miscreg1,MiscRegOffMSP(1)(atmp1)
	lwz	miscreg2,MiscRegOffMSP(2)(atmp1)
	lwz	miscreg3,MiscRegOffMSP(3)(atmp1)
	lwz	miscreg4,MiscRegOffMSP(4)(atmp1)
	lwz	miscreg5,MiscRegOffMSP(5)(atmp1)
	lwz	miscreg6,MiscRegOffMSP(6)(atmp1)
	lwz	miscreg7,MiscRegOffMSP(7)(atmp1)
	lwz	miscreg8,MiscRegOffMSP(8)(atmp1)
	lwz	miscreg9,MiscRegOffMSP(9)(atmp1)
	lwz	miscreg10,MiscRegOffMSP(10)(atmp1)
	lwz	miscreg11,MiscRegOffMSP(11)(atmp1)
	lwz	miscreg12,MiscRegOffMSP(12)(atmp1)
	lwz	miscreg13,MiscRegOffMSP(13)(atmp1)
	lwz	miscreg14,MiscRegOffMSP(14)(atmp1)
	lwz	stdlink,LinkRegOffMSP(atmp1)
	lwz	varptr,VarPtrOffMSP(atmp1)
	lwz	basereg,BasePtrOffMSP(atmp1)
	lwz	gclink,PCOffMSP(atmp1)
	addi	basereg,basereg,32764		/* adjust baseReg */
						/* check for pending signals */
	lwz	atmp1,NPendingSysOffVSP(atmp2)
	lwz	atmp3,NPendingOffVSP(atmp2)
	add	atmp1,atmp1,atmp3
	cmpi	CR0,atmp1,0
	bf	CR0_EQ,pending_sigs


ENTRY(ml_go) 
	cmpl	CR0,limitptr,allocptr
	mtlr	gclink

	mtfsfi  3,0			/* Ensure that no exceptions are set */
	mtfsfi  2,0
	mtfsfi  1,0
	mtfsfi  0,0
	li	0,0
	mtxer   0
	blr				/* jump to ML code */

pending_sigs:				/* there are pending signals */
	lwz	atmp1,InSigHandlerOffVSP(atmp2)
	cmpi	CR0,atmp1,0
	bf 	CR0_EQ,CSYM(ml_go)
					/* check if currently handling a signal */	
	lwz	atmp1,InSigHandlerOffVSP(atmp2)
	cmpi	CR0,atmp1,0
	bf	CR0_EQ,CSYM(ml_go)

	li	0,1
	stw	0,HandlerPendingOffVSP(atmp2)
	addi	limitptr,allocptr,0
	b	CSYM(ml_go)


ML_CODE_HDR(array_a)
	CHECKLIMIT(FUN_MASK,array_a_limit)
	lwz	atmp2,0(stdarg)		/* atmp2 := tagged length */
	srawi	atmp2,atmp2,1		/* atmp2 := untagged length */
 	cmpi	CR0,atmp2,SMALL_OBJ_SZW /* is this a large object */
	bf	CR0_LT,array_a_large	
	slwi	atmp3,atmp2,TAG_SHIFTW  /* atmp3 := build descriptor */
	ori	atmp3,atmp3,MAKE_TAG(DTAG_array)
	stw	atmp3,0(allocptr)	/* store the descriptor */
	addi	allocptr,allocptr,4	/* points to new object */
	lwz	atmp4,4(stdarg)		/* atmp4 := get initial value */
	addi	stdarg,allocptr,0	/* put ptr in return register */
	slwi	atmp2,atmp2,2		/* atmp2 := length in bytes */
	add	atmp1,atmp2,allocptr	/* beyond last word of new array */
array_a_2:				/* loop */
        addi	allocptr,allocptr,4	/* on to the next word */
	cmp	CR0,allocptr,atmp1
	stw	atmp4,-4(allocptr)	/* store the value */
	bf	CR0_EQ, array_a_2	/* if not off the end, repeat */
					/* end loop */
	CONTINUE
array_a_large:				/* off-line allocation */
	li	atmp1,FUN_MASK
	li	atmp4,REQ_ALLOC_ARRAY
	b	set_request


ML_CODE_HDR(create_b_a)
	CHECKLIMIT(FUN_MASK,create_b_a_limit)
	srawi	atmp2,stdarg,1		/* atmp1 = length */
	addi	atmp2,atmp2,3
	srawi	atmp2,atmp2,2		/* length in words (including desc) */
	cmpi    CR0,atmp2,SMALL_OBJ_SZW /* is this a small object */
	bf     CR0_LT,create_b_a_large

	srawi	atmp3,stdarg,1		/* build descriptor in atmp3 */
	slwi	atmp3,atmp3,TAG_SHIFTW
	ori	atmp3,atmp3,MAKE_TAG(DTAG_bytearray)
	stw	atmp3,0(allocptr)	/* store descriptor */
	addi	stdarg,allocptr,4	/* pointer to new string */
	slwi	atmp2,atmp2,2		/* length in bytes */
	addi	atmp2,atmp2,4		/* length + tag */
	add	allocptr,allocptr,atmp2	/* advance allocptr */
	CONTINUE

create_b_a_large:
	li	atmp1,FUN_MASK
	li 	atmp4,REQ_ALLOC_BYTEARRAY
	b	set_request


/*
** create_s_a: int -> string
*/
ML_CODE_HDR(create_s_a)
	CHECKLIMIT(FUN_MASK,create_s_a_limit)
	srawi	atmp2,stdarg,1		/* atmp1 = length */
	addi	atmp2,atmp2,4
	srawi	atmp2,atmp2,2		/* length in words (including desc) */
	cmpi	CR0,atmp2,SMALL_OBJ_SZW /* is this a small object */
	bf	CR0_LT,create_s_a_large
	
	srawi	atmp1,stdarg,1		/* build descriptor in atmp1 */
	slwi	atmp1,atmp1,TAG_SHIFTW
	ori	atmp1,atmp1,MAKE_TAG(DTAG_string)
	stw	atmp1,0(allocptr)	/* store descriptor */
	addi	stdarg,allocptr,4	/* pointer to new string */
	slwi	atmp2,atmp2,2		/* length in bytes */
	addi	atmp2,atmp2,4		/* + tag */
	add	allocptr,allocptr,atmp2	/* advance allocptr */
	li	0,0
	stw	0,-4(allocptr)		/* zero in last word */
	CONTINUE

create_s_a_large:
	li	atmp1,FUN_MASK
	li	atmp4,REQ_ALLOC_STRING
	b	set_request



ML_CODE_HDR(create_r_a)
	CHECKLIMIT(FUN_MASK,create_r_a_limit)
	srawi	atmp2,stdarg,1		/* atmp1 = length */
	slwi	atmp2,atmp2,1		/* length in words */
	cmpi	CR0,atmp2,SMALL_OBJ_SZW	/* is this a small object */
	bf	CR0_LT,create_r_a_large
	
	srawi	atmp3,stdarg,1		/* descriptor in atmp3 */
	slwi	atmp3,atmp3,TAG_SHIFTW
	ori	atmp3,atmp3,MAKE_TAG(DTAG_realdarray)
#ifdef ALIGN_REALDS
	ori	allocptr,allocptr,4
#endif	
	stw	atmp3,0(allocptr)
	addi	stdarg,allocptr,4	/* pointer to new realarray */
	slwi	atmp2,atmp2,2		/* length in bytes */
	addi	atmp2,atmp2,4		/* plus tag */
	add	allocptr,allocptr,atmp2	/* new allocptr */
	CONTINUE
create_r_a_large:
	li	atmp1,FUN_MASK
	li	atmp4,REQ_ALLOC_REALDARRAY
	b	set_request


ML_CODE_HDR(create_v_a)
	CHECKLIMIT(FUN_MASK,create_v_a_limit)
	lwz	atmp1,0(stdarg)		/* atmp1 := tagged length */
	srawi	atmp1,atmp1,1		/* untagged length */
	cmpi	CR0,atmp1,SMALL_OBJ_SZW /* is this a small object */
	bf	CR0_LT,create_v_a_large

	slwi	atmp2,atmp1,TAG_SHIFTW	/* build descriptor in atmp2 */
	ori	atmp2,atmp2,MAKE_TAG(DTAG_vector)
	stw	atmp2,0(allocptr)
	addi	allocptr,allocptr,4
	lwz	atmp2,4(stdarg)		/* atmp2 := list */
	addi	stdarg,allocptr,0	/* stdarg := vector */
	li	atmp3,ML_nil

create_v_a_1:
	lwz	atmp1,0(atmp2)		/* atmp1:=hd(atmp2) */
	lwz	atmp2,4(atmp2)		/* atmp2:=tl(atmp2) */
	cmp	CR0,atmp2,atmp3
	stw	atmp1,0(allocptr)	/* store word */
	addi	allocptr,allocptr,4
	bf	CR0_EQ,create_v_a_1

	CONTINUE

create_v_a_large:
	li	atmp1,FUN_MASK
	li	atmp4,REQ_ALLOC_VECTOR
	b	set_request


#if defined(USE_TOC)
	.toc
T.floor_CONST:
	.tc	H.floor_CONST[TC],floor_CONST
#endif
	RO_DATA
	ALIGN8
floor_CONST:
	DOUBLE(4512395720392704.0)

	TEXT
	/*
	** floor_a : real -> int
	**	Do not test for overflow, it's the caller's
	**	responsibility to be in range.
	**
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
	lwz	0, 4(allocptr)
	mtfsb1	30
	mtfsb1 	31
#ifdef USE_TOC
	lwz	atmp1, T.floor_CONST(2)
	lfd	3, 0(atmp1)
#else
	lis	atmp1, floor_CONST@ha
	lfd	3, floor_CONST@l(atmp1)
#endif
	fadd	6,1,3
	stfd	6,FLOOR_OFFSET(sp)
	lwz	stdarg,FLOOR_OFFSET+4(sp)
	add	stdarg,stdarg,stdarg
	addi	stdarg,stdarg,1
	
	andi.	0,0, 0xf
	mtfsf	0xff,0
	CONTINUE


ML_CODE_HDR(logb_a)
	lwz	stdarg,0(stdarg)  	/* most significant part */
	srawi 	stdarg,stdarg,20	/* throw out 20 low bits */
	andi.	stdarg,stdarg,0x07ff	/* clear all but 11 low bits */
	addi	stdarg,stdarg,-1023	/* subtract 1023 */
	slwi	stdarg,stdarg,1		/* make room for tag bit */
	addi	stdarg,stdarg,1		/* add the tag bit */
	CONTINUE


/*
** scalb : real * int -> real
**	scalb(x,y) = x * 2^y
*/
ML_CODE_HDR(scalb_a)
	CHECKLIMIT(FUN_MASK,scalb_v_limit)
	lwz	atmp1,4(stdarg)		/* atmp1 := y */
	srawi	atmp1,atmp1,1		/* atmp1 := machine int y */
	lwz	stdarg,0(stdarg)	/* stdarg := x */
	lwz	atmp2,0(stdarg)		/* atmp2 := MSW(x) */
	lis	0,0x7ff0		/* r0 := 0x7ff0,0000 */
	and.	atmp3,atmp2,0		/* atmp3 := atmp2 & 0x7ff00000 */
	bt	CR0_EQ,scalb_all_done
	
	srawi	atmp3,atmp3,20		/* atmp3 := ieee(exp) */
	add.	atmp1,atmp1,atmp3	/* scale exponent */
	bt	CR0_LT,scalb_underflow

	cmpi	CR0,atmp1,2047		/* max. ieee(exp) */
	bf	CR0_LT,scalb_overflow

	not	0,0			/* r0 := not(r0) */
	and	atmp2,atmp2,0		/* atmp2 := high mantessa bits + sign */
	slwi	atmp1,atmp1,20		/* atmp1 := new exponent */
	or	atmp1,atmp1,atmp2	/* atmp1 := new MSB(x) */
	lwz	atmp2, 4(stdarg)	

scalb_write_out:
	stw	atmp1, 4(allocptr)
	stw	atmp2, 8(allocptr)
	li	atmp3, DESC_reald
	stw	atmp3, 0(allocptr)
	addi	stdarg,allocptr,4
	addi	allocptr,allocptr,12

scalb_all_done:
	CONTINUE

scalb_underflow:
	li	atmp1,0
	li	atmp2,0
	b	scalb_write_out

LABEL(scalb_overflow)
	mtfsb1 	3



ML_CODE_HDR(try_lock_a)
	lwz	atmp1,0(stdarg)
	li	atmp2,1			/* ML_false */
	stw	atmp2,0(stdarg)
	addi	stdarg,atmp1,0
	CONTINUE


ML_CODE_HDR(unlock_a)
	li	atmp1,3			/* ML_true */
	stw	atmp1,0(stdarg)
	li	stdarg,1		/* just return unit */
	CONTINUE



/* saveFPRegs and restoreFPRegs are called from C only. */
#define ctmp1 12
#define ctmp2 11
#define ctmp3 10


CENTRY(SaveFPRegs)
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

	blr

CENTRY(RestoreFPRegs)
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
	blr

#if defined(OPSYS_MKLINUX)

#define CACHE_LINE_SZB		32
#define CACHE_LINE_MASK		(CACHE_LINE_SZB-1)
#define CACHE_LINE_BITS		26

/* FlushICache:
 *
 *   void FlushICache (Addr_t addr, Addr_t nbytes)
 */
CENTRY(FlushICache)
	add	4,3,4			/* stop := addr+nbytes */
	addic	4,4,CACHE_LINE_MASK	/* stop := stop + CACHE_LINE_MASK */
	rlwinm	4,4,0,0,CACHE_LINE_BITS	/* stop := stop & ~CACHE_LINE_MASK */
L_FlushICache_1:
	cmplw	1,3,4			/* while (addr < stop) */
	bc	4,4,L_FlushICache_2
	dcbf	0,3			/*   flush addr */
	icbi	0,3			/*   invalidate addr */
	addi	3,3,CACHE_LINE_SZB	/*   addr := addr + CACHE_LINE_SZB */
	b	L_FlushICache_1		/* end while */
L_FlushICache_2:
	blr

#endif

