/* X86.prim.asm
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This was derived from I386.prim.s, by Mark Leone (mleone@cs.cmu.edu)
 */

#include "ml-base.h"
#include "asm-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-request.h"
#include "reg-mask.h"
#include "ml-limits.h"

/* enable/disable virtual (memory-based) registers.
 * the number of vregs, etc. must concur with:
 *	src/runtime/include/ml-state.h
 *	src/runtime/kernel/ml-state.c
 *      src/sml-nj/x86/x86.sml
 */

#ifndef VREGS
#  define VREGS
#endif

/*
 *
 * The 386 registers are used as follows:
 *
 * EAX - temp1 (see the code generator, x86/x86.sml)
 * EBX - misc1
 * ECX - misc2
 * EDX - misc3
 * ESI - standard continuation (ml_cont, see ml_state.h)
 * EBP - standard argument (ml_arg)
 * EDI - free space pointer (ml_allocptr)
 * ESP - stack pointer
 * EIP - program counter (ml_pc)
 */

/* Registers (see x86/x86.sml): */
#define temp		%eax
#define misc1		%ebx
#define misc2		%ecx
#define misc3		%edx
#define stdcont		%esi
#define stdarg		%ebp
#define allocptr	%edi

/* other reg uses */
#define creturn 	%eax

/*
 * Other values, which on most architectures would reside in registers,
 * are stored on the stack:
 * 
 * 0(ESP) - tempmem (used by the X86 code generator)
 * 4(ESP) - tempmem2 (used by the X86 code generator)
 * 8(ESP) - exception handler continuation (ml_exncont)
 * 12(ESP) - data limit (ml_limitptr)
 * 16(ESP) - standard closure (ml_closure)
 * 20(ESP) - link register (ml_linkreg)
 * 24(ESP) - store pointer (ml_storeptr)
 * 28(ESP) - var pointer (ml_varptr)
 *
 */

/* Stack frame (see x86/x86.sml): */
#define tempmem		0(%esp)
#define tempmem2	4(%esp)
#define exncont		8(%esp)
#define limitptr	12(%esp)
#define stdclos		16(%esp)
#define stdlink		20(%esp)
#define storeptr	24(%esp)
#define varptr		28(%esp)
#define start_gc	32(%esp)
#define mask		36(%esp)
#define vreg0		40(%esp)
#define vreg1		44(%esp)
#define vreg2		48(%esp)
#define vreg3		52(%esp)
#define vreg4		56(%esp)
#define vreg5		60(%esp)
#define vreg6		64(%esp)
#define vreg7		68(%esp)
#define vreg8		72(%esp)
#define vreg9		76(%esp)
#define vreg10		80(%esp)
#define vreg11		84(%esp)
#define vreg12		88(%esp)     /* unused */ /* used as pseudo reg */
#define vreg13		92(%esp)     /* unused */ /* used as pseudo reg */
#define vreg14		96(%esp)     /* unused */
#define vreg15		100(%esp)    /* unused */
#define mlstate_ptr	104(%esp)
#define ML_STATE_OFFSET 104
#define ML_FRAME_SIZE	(ML_STATE_OFFSET+4)


#define PSEUDOREG_1	vreg12
#define PSEUDOREG_2 	vreg13

#define	via	*


	DATA
	ALIGN4
tempmem_w:		/* temp word for the code generator */
	.long 0
tempmem2_w:		/* another temp word for the code generator */
	.long 0
request_w:		/* place to put the request code */
	.long 0
	GLOBAL(ML_X86Frame)
LABEL(CSYM(ML_X86Frame)) /* ptr to the ml frame (gives C access to limitptr) */
	.long 0		

/*
 * Note that the garbage collector only preserves root registers 
 * (EBX, ECX, EDX, ESI, EBP, EIP).
 */


#include "mlstate-offsets.h"	/** this file is generated **/


/*
 * 386 function call conventions:  
 *  [true for gcc and dynix3 cc; untested for others]
 *
 * 	Caller save registers: eax, ecx, edx
 * 	Callee save registers: ebx, esi, edi, and ebp. 
 * 	Floating point state is caller-save.
 * 	Arguments passed on stack.  Rightmost argument pushed first.
 * 	Word-sized result returned in %eax.
 */

#define cresult	%eax

#define CALLEE_SAVE	\
	pushl	%ebx;	\
	pushl	%esi;	\
	pushl	%edi;	\
	pushl	%ebp	

#define CALLEE_RESTORE	\
	popl	%ebp;	\
	popl	%edi;	\
	popl	%esi;	\
	popl	%ebx 

/* MOVE copies one memory location to another, using a specified temporary. */

#define MOVE(src,tmp,dest)	\
	movl	src, tmp;	\
	movl	tmp, dest

#if (CALLEESAVE > 0)
#define CONTINUE						\
	cmpl	limitptr, allocptr;				\
	jmp	via stdcont
#else
#define CONTINUE							\
	movl	(stdcont), temp;					\
	movl	temp, stdlink;	  	/* Not really a register */	\
	cmpl	limitptr, allocptr;					\
	jmp     via temp
#endif

#define CHECKLIMIT(maskval)						\
 1:;									\
	jb	9f;							\
	lea	1b, temp;		/* temp holds resume address */	\
	movl	IMMED(maskval), mask;					\
	jmp	CSYM(saveregs);						\
 9:

/**********************************************************************/
	TEXT
	ALIGN4

ML_CODE_HDR(sigh_return_a)
	movl	mlstate_ptr, temp
	movl	IMMED(REQ_SIG_RETURN), request_w
	movl	IMMED(RET_MASK), mask
	jmp	CSYM(set_request)

/* sigh_resume:
 * Resume execution at the point at which a handler trap occurred.  This is a
 * standard two-argument function, thus the closure is in ml_cont.
 */

ENTRY(sigh_resume)
	movl	mlstate_ptr, temp
	movl	IMMED(REQ_SIG_RESUME), request_w
/*	movl	IMMED(RET_MASK), mask
 */
	movl	IMMED(FUN_MASK), mask
	jmp	CSYM(set_request)

/* pollh_return_a:
 * The return continuation for the ML poll handler.
 */
ML_CODE_HDR(pollh_return_a)
	movl	mlstate_ptr, temp
	movl	IMMED(REQ_POLL_RETURN), request_w
	movl	IMMED(RET_MASK), mask
	jmp	CSYM(set_request)

/* pollh_resume:
 * Resume execution at the point at which a poll event occurred.
 */
ENTRY(pollh_resume)
	movl	mlstate_ptr, temp
	movl	IMMED(REQ_POLL_RESUME), request_w
/*	movl	IMMED(RET_MASK), mask
 */
	movl	IMMED(FUN_MASK), mask
	jmp	CSYM(set_request)

ML_CODE_HDR(handle_a)
	movl	mlstate_ptr, temp
	movl	IMMED(REQ_EXN), request_w
	movl	IMMED(EXN_MASK), mask
	jmp	CSYM(set_request)

ML_CODE_HDR(return_a)
	movl	mlstate_ptr, temp
	movl	IMMED(REQ_RETURN), request_w
	movl	IMMED(RET_MASK), mask
	jmp	CSYM(set_request)

/* Request a fault.  The floating point coprocessor must be reset
 * (thus trashing the FP registers) since we don't know whether a 
 * value has been pushed into the temporary "register".	 This is OK 
 * because no floating point registers will be live at the start of 
 * the exception handler.
 */
ENTRY(request_fault)
	call    CSYM(FPEEnable)          /* Doesn't trash any general regs. */
	movl	mlstate_ptr, temp
	movl	IMMED(REQ_FAULT), request_w
	movl	IMMED(EXN_MASK), mask
	jmp	CSYM(set_request)

/* bind_cfun : (string * string) -> c_function
 */
ML_CODE_HDR(bind_cfun_a)
	CHECKLIMIT(FUN_MASK)
	movl	mlstate_ptr, temp
	movl	IMMED(REQ_BIND_CFUN), request_w
	movl	IMMED(FUN_MASK), mask
	jmp	CSYM(set_request)

ML_CODE_HDR(build_literals_a)
	CHECKLIMIT(FUN_MASK)
	movl	mlstate_ptr, temp
	movl	IMMED(REQ_BUILD_LITERALS), request_w
	movl	IMMED(FUN_MASK), mask
	jmp	CSYM(set_request)

ML_CODE_HDR(callc_a)
	CHECKLIMIT(FUN_MASK)
	movl	mlstate_ptr, temp
	movl	IMMED(REQ_CALLC), request_w
	movl	IMMED(FUN_MASK), mask
	/* fall into set_request */

ENTRY(set_request)
	/* temp holds mlstate_ptr, valid request in request_w  */
	/* Save registers */
	movl	allocptr,AllocPtrOffMSP(temp)
	movl	stdarg,StdArgOffMSP(temp)
	movl	stdcont,StdContOffMSP(temp)

#define	temp2 allocptr
	/* note that we have left ML code */
	movl	VProcOffMSP(temp),temp2
	movl	IMMED(0), InMLOffVSP(temp2)	

#if (CALLEESAVE > 0)
	movl	misc1, MiscRegOffMSP(0)(temp)
#if (CALLEESAVE > 1)
	movl	misc2, MiscRegOffMSP(1)(temp)
#if (CALLEESAVE > 2)
	movl	misc3, MiscRegOffMSP(2)(temp)

	/* Save vregs before the stack frame is popped. */

#if (CALLEESAVE > 3)
 	MOVE(vreg0, temp2, MiscRegOffMSP(3)(temp))
#if (CALLEESAVE > 4)
 	MOVE(vreg1, temp2, MiscRegOffMSP(4)(temp))
#if (CALLEESAVE > 5)
 	MOVE(vreg2, temp2, MiscRegOffMSP(5)(temp))
#if (CALLEESAVE > 6)
 	MOVE(vreg3, temp2, MiscRegOffMSP(6)(temp))
#if (CALLEESAVE > 7)
 	MOVE(vreg4, temp2, MiscRegOffMSP(7)(temp))
#if (CALLEESAVE > 8)
 	MOVE(vreg5, temp2, MiscRegOffMSP(8)(temp))
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif

	MOVE(tempmem, temp2, tempmem_w)
	MOVE(tempmem2,temp2, tempmem2_w)
	MOVE(limitptr,temp2, LimitPtrOffMSP(temp))
	MOVE(exncont, temp2, ExnPtrOffMSP(temp)) 
	MOVE(stdclos, temp2, StdClosOffMSP(temp))
	MOVE(stdlink, temp2, LinkRegOffMSP(temp))
	MOVE(stdlink, temp2, PCOffMSP(temp))
	MOVE(storeptr,temp2, StorePtrOffMSP(temp))
	MOVE(varptr,  temp2, VarPtrOffMSP(temp))
	MOVE(mask,    temp2, MaskOffMSP(temp))

	/* pseudo regs */
	MOVE(PSEUDOREG_1,temp2,PseudoReg1OffMSP(temp))
	MOVE(PSEUDOREG_2,temp2,PseudoReg2OffMSP(temp))
#undef	temp2	
	
	/* return val of function is request code */
	movl	request_w,creturn

	/* Pop the stack frame and return to run_ml(). */
	addl	IMMED(ML_FRAME_SIZE), %esp
	CALLEE_RESTORE
	ret

	TEXT
	ALIGN4

ENTRY(saveregs)
	pushl	temp			/* Contains "resume" address. */
	movl	ML_STATE_OFFSET+4(%esp), temp
	popl	PCOffMSP(temp)

#ifdef SOFT_POLL
	/* free some regs */
	movl	misc1, MiscRegOffMSP(0)(temp)
	movl	misc2, MiscRegOffMSP(1)(temp)
#define tmpR	misc1
#define pfreq	misc2
	/* check if polling enabled (PollFreq > 0) */
	lea	CSYM(_PollFreq0),pfreq		/* load contents of ref */
	movl	4(pfreq),pfreq			
	shrl	IMMED(1),pfreq			/* strip integer tag */
	jz	check_for_gc			/* go check for real gc */
	cmpl	IMMED(0),InPollHandlerOffMSP(temp)    /* if we're in the handler */
	jne	reset_limit			/* ignore poll events */
	lea	CSYM(_PollEvent0),tmpR		/* load contents of ref */
	movl	4(tmpR),tmpR
	shrl	IMMED(1),tmpR
	jz	reset_limit			/* check for poll event */
	/* event occurred, so set ml_pollHandlerPending */
	movl	IMMED(1),PollPendingOffMSP(temp)
	jmp	do_gc		/* and handle event in the C runtime */
	
reset_limit:	/* reset limit ptr */
	shll	IMMED(POLL_GRAIN_BITS),pfreq		/* mult by POLL_GRAIN_CPSI */
	movl	allocptr,limitptr
	addl	pfreq,limitptr
#undef	pfreq

check_for_gc:
	/* ensure real limit is >= limit */
	movl	RealLimitOffMSP(temp),tmpR
	cmpl	limitptr,tmpR
	ja	ok_limit
	movl	tmpR,limitptr
ok_limit:
	addl	IMMED(-4096),limitptr
	cmpl	limitptr,allocptr
	jge	do_gc		       	/* gc *//* should be a common case */
	addl	IMMED(4096),limitptr
	/* since a signal also sets limitptr == allocptr to force a trap, */
	/* we need to disambiguate poll-events/signals here */
#define	vsp	misc2
	movl	IMMED(0),tmpR
	addl	PollPendingOffMSP(temp),tmpR
	movl	VProcOffMSP(temp),vsp
	addl	NPendingOffVSP(vsp),tmpR
	addl	NPendingSysOffVSP(vsp),tmpR
	jnz	do_gc
#undef  vsp

no_gc:	/* an uneventful poll check, back to ML */
	movl	MiscRegOffMSP(0)(temp),misc1
	movl	MiscRegOffMSP(1)(temp),misc2
	movl	PCOffMSP(temp),temp
	cmpl	limitptr, allocptr
	jmpl	via temp

do_gc:
	/* limitptr saved below */

#undef tmpR
#endif /* SOFT_POLL */


	/* Save registers. */
	movl	allocptr, AllocPtrOffMSP(temp)
	movl	stdarg, StdArgOffMSP(temp)
	movl	stdcont, StdContOffMSP(temp)
#ifndef SOFT_POLL  /* misc1 & misc2 saved above for SOFT_POLL */
	movl	misc1, MiscRegOffMSP(0)(temp)
	movl	misc2, MiscRegOffMSP(1)(temp)
#endif
	movl	misc3, MiscRegOffMSP(2)(temp)

#define	temp2 allocptr

	/* note that we have left ML code */
	movl	VProcOffMSP(temp),temp2
	movl	IMMED(0), InMLOffVSP(temp2)	

	/* vregs */
#ifdef VREGS
	MOVE(vreg0, temp2, MiscRegOffMSP(3)(temp))
	MOVE(vreg1, temp2, MiscRegOffMSP(4)(temp))
	MOVE(vreg2, temp2, MiscRegOffMSP(5)(temp))
	MOVE(vreg3, temp2, MiscRegOffMSP(6)(temp))
	MOVE(vreg4, temp2, MiscRegOffMSP(7)(temp))
	MOVE(vreg5, temp2, MiscRegOffMSP(8)(temp))
	MOVE(vreg6, temp2, MiscRegOffMSP(9)(temp))
	MOVE(vreg7, temp2, MiscRegOffMSP(10)(temp))
	MOVE(vreg8, temp2, MiscRegOffMSP(11)(temp))
	MOVE(vreg9, temp2, MiscRegOffMSP(12)(temp))
	MOVE(vreg10, temp2, MiscRegOffMSP(13)(temp))
	MOVE(vreg11, temp2, MiscRegOffMSP(14)(temp))
#endif

	MOVE(tempmem, temp2, tempmem_w)
	MOVE(tempmem2,temp2, tempmem2_w)
	MOVE(exncont, temp2, ExnPtrOffMSP(temp)) 
	MOVE(stdclos, temp2, StdClosOffMSP(temp))
	MOVE(stdlink, temp2, LinkRegOffMSP(temp))
	MOVE(storeptr,temp2, StorePtrOffMSP(temp))
	MOVE(limitptr,temp2, LimitPtrOffMSP(temp))
	MOVE(varptr,  temp2, VarPtrOffMSP(temp))
	MOVE(mask,    temp2, MaskOffMSP(temp))

	/* pseudo regs */
	MOVE(PSEUDOREG_1,temp2,PseudoReg1OffMSP(temp))
	MOVE(PSEUDOREG_2,temp2,PseudoReg2OffMSP(temp))
#undef	temp2	

	/* Pop the stack frame and return to run_ml(). */
	movl	IMMED(REQ_GC),creturn
	addl	IMMED(ML_FRAME_SIZE), %esp
	CALLEE_RESTORE
	ret

ENTRY(restoreregs)
	movl	4(%esp), temp		/* Get argument (MLState ptr). */
	CALLEE_SAVE

#define temp2	%ebx
	/* Allocate and initialize the ML stack frame. */
	subl	IMMED(ML_FRAME_SIZE), %esp
	MOVE(	tempmem_w,  temp2, tempmem)
	MOVE(	tempmem2_w, temp2, tempmem2)
	MOVE(	ExnPtrOffMSP(temp),  temp2, exncont) 
	MOVE(	LimitPtrOffMSP(temp), temp2, limitptr)
	MOVE(	StdClosOffMSP(temp),  temp2, stdclos)
	MOVE(	LinkRegOffMSP(temp),  temp2, stdlink) 
	MOVE(	StorePtrOffMSP(temp), temp2, storeptr)
	MOVE(	VarPtrOffMSP(temp),   temp2, varptr)
	MOVE(	MaskOffMSP(temp),   temp2, mask)
	lea	CSYM(saveregs), temp2
	movl	temp2,start_gc
	movl	temp, mlstate_ptr

	/* vregs */
#ifdef VREGS
	MOVE(MiscRegOffMSP(3)(temp),temp2,vreg0) 
	MOVE(MiscRegOffMSP(4)(temp),temp2,vreg1) 
	MOVE(MiscRegOffMSP(5)(temp),temp2,vreg2) 
	MOVE(MiscRegOffMSP(6)(temp),temp2,vreg3) 
	MOVE(MiscRegOffMSP(7)(temp), temp2, vreg4)
	MOVE(MiscRegOffMSP(8)(temp), temp2, vreg5)
	MOVE(MiscRegOffMSP(9)(temp), temp2, vreg6)
	MOVE(MiscRegOffMSP(10)(temp), temp2, vreg7)
	MOVE(MiscRegOffMSP(11)(temp), temp2, vreg8)
	MOVE(MiscRegOffMSP(12)(temp), temp2, vreg9)
	MOVE(MiscRegOffMSP(13)(temp), temp2, vreg10)
	MOVE(MiscRegOffMSP(14)(temp), temp2, vreg11)
#endif
	/* pseudo regs */
	MOVE(PseudoReg1OffMSP(temp),temp2,PSEUDOREG_1)
	MOVE(PseudoReg2OffMSP(temp),temp2,PSEUDOREG_2)

#undef	temp2

	/* Load ML registers. */
	movl	AllocPtrOffMSP(temp), allocptr
	movl	StdContOffMSP(temp), stdcont
	movl	StdArgOffMSP(temp), stdarg
	movl	MiscRegOffMSP(0)(temp), misc1
	movl	MiscRegOffMSP(1)(temp), misc2
	movl	MiscRegOffMSP(2)(temp), misc3

	movl	%esp,CSYM(ML_X86Frame)	/* frame ptr for signal handler. */

	pushl	misc2			/* free up a register   */
	pushl	temp			/* save msp temporarily */

#define	tmpreg	misc2

	/* note that we're entering ML */
	movl	VProcOffMSP(temp),temp  /* temp is now vsp */
#define vsp	temp
	movl	IMMED(1),InMLOffVSP(vsp)

	/* handle signals */
	movl	NPendingSysOffVSP(vsp),tmpreg
	addl	NPendingOffVSP(vsp),tmpreg
	cmpl	IMMED(0),tmpreg
#undef  tmpreg
	jne	pending

restore_and_jmp_ml:
	popl	temp			/* restore temp to msp */
	popl	misc2
	
jmp_ml:
	movl	PCOffMSP(temp),temp
	cmpl	limitptr, allocptr
	jmpl	via temp		/* Jump to ML code. */

pending:
	cmpl	IMMED(0),InSigHandlerOffVSP(vsp)   /* Currently handling signal? */
	jne	restore_and_jmp_ml
	movl	IMMED(1),HandlerPendingOffVSP(vsp) /* handler trap is now pending */

	/* must restore here because limitptr is on stack */
	popl	temp			/* restore temp to msp */
	popl	misc2

	movl	allocptr,limitptr
	jmp	jmp_ml			/* Jump to ML code. */

#undef  vsp

/* ----------------------------------------------------------------------
 * array : (int * 'a) -> 'a array
 * Allocate and initialize a new array.	 This can cause GC.
 */

ML_CODE_HDR(array_a)
	CHECKLIMIT(FUN_MASK)
	movl 	0(stdarg),temp               /* desired length into temp */
	sarl	IMMED(1),temp			     /* untagged */
	cmpl	IMMED(SMALL_OBJ_SZW),temp
	jge	3f

#define	tmpreg	misc1
	pushl	tmpreg

	movl	temp,tmpreg		     /* build descriptor in tmpreg */
	sall	IMMED(TAG_SHIFTW),tmpreg
	orl	IMMED(MAKE_TAG(DTAG_array)),tmpreg
	movl	tmpreg,0(allocptr)	     /* write descriptor */
	addl	IMMED(4),allocptr
	movl	4(stdarg),tmpreg	     /* initial values */
	movl	allocptr,stdarg		     /* stdarg gets ptr to new array */
	sall	IMMED(2),temp			     /* length in bytes */
	addl	allocptr,temp		     
	xchgl	tmpreg,temp		     /* tmpreg is end of array */
2:					     /* loop: */
	stosl					/* 0(allocptr++) <- temp  */
	cmpl	allocptr,tmpreg			/* check for end of array */
	jne	2b

	popl	tmpreg
#undef  tmpreg

	CONTINUE
3:
	movl	mlstate_ptr, temp
	movl	IMMED(REQ_ALLOC_ARRAY), request_w
	movl	IMMED(FUN_MASK), mask
	jmp	CSYM(set_request)
	

/* create_r : int -> realarray */
ML_CODE_HDR(create_r_a)
	CHECKLIMIT(FUN_MASK)
	movl 	stdarg,temp               /* desired length into temp */
	sarl	IMMED(1),temp			  /* untagged */
	shll	IMMED(1),temp			  /* size in words */
	cmpl	IMMED(SMALL_OBJ_SZW),temp
	jge	2f

#define	tmpreg	misc1
	pushl	tmpreg

	shrl	IMMED(1),temp			  /* size in reals */
	movl	temp,tmpreg		     /* build descriptor in tmpreg */
	sall	IMMED(TAG_SHIFTW),tmpreg
	orl	IMMED(MAKE_TAG(DTAG_realdarray)),tmpreg
	movl	tmpreg,0(allocptr)	     /* write descriptor */
	addl	IMMED(4),allocptr
	movl	allocptr,stdarg		     /* stdarg gets ptr to new array */
	sall	IMMED(3),temp			     /* length in bytes */
	addl	temp,allocptr		     /* adjust allocptr past array */

	popl	tmpreg
#undef  tmpreg
	CONTINUE
2:
	movl	mlstate_ptr, temp
	movl	IMMED(REQ_ALLOC_REALDARRAY), request_w
	movl	IMMED(FUN_MASK), mask
	jmp	CSYM(set_request)


/* create_b : int -> bytearray */
ML_CODE_HDR(create_b_a)
	CHECKLIMIT(FUN_MASK)
	movl 	stdarg,temp                  /* the length */
	sarl	IMMED(1),temp			     /* untagged */
	addl	IMMED(3),temp			     /* round */	
	sarl	IMMED(2),temp			     /* to words */
	cmpl	IMMED(SMALL_OBJ_SZW),temp
	jge	2f

#define	tmpreg	misc1
	pushl	tmpreg

	movl	stdarg,tmpreg		     /* build descriptor in tmpreg */
	sarl	IMMED(1),tmpreg
	sall	IMMED(TAG_SHIFTW),tmpreg
	orl	IMMED(MAKE_TAG(DTAG_bytearray)),tmpreg
	movl	tmpreg,0(allocptr)	     /* write descriptor */
	addl	IMMED(4),allocptr
	movl	allocptr,stdarg		     /* stdarg gets ptr to new str */
	sall	IMMED(2),temp			     /* length in bytes (untagged) */
	addl	temp,allocptr		     /* allocptr += total length */

	popl	tmpreg
#undef  tmpreg

	CONTINUE
2:
	movl	mlstate_ptr, temp
	movl	IMMED(REQ_ALLOC_BYTEARRAY), request_w
	movl	IMMED(FUN_MASK), mask
	jmp	CSYM(set_request)


/* create_s : int -> string */
ML_CODE_HDR(create_s_a)
	CHECKLIMIT(FUN_MASK)
	movl 	stdarg,temp                  /* the length */
	sarl	IMMED(1),temp			     /* untagged */
	addl	IMMED(4),temp			     /* round */	
	sarl	IMMED(2),temp			     /* to words */
	cmpl	IMMED(SMALL_OBJ_SZW),temp
	jge	2f

#define	tmpreg	misc1
	pushl	tmpreg

	movl	stdarg,tmpreg		     /* build descriptor in tmpreg */
	sarl	IMMED(1),tmpreg
	sall	IMMED(TAG_SHIFTW),tmpreg
	orl	IMMED(MAKE_TAG(DTAG_string)),tmpreg
	movl	tmpreg,0(allocptr)	     /* write descriptor */
	addl	IMMED(4),allocptr
	movl	allocptr,stdarg		     /* stdarg gets ptr to new str */
	sall	IMMED(2),temp			     /* length in bytes (untagged) */
	addl	temp,allocptr		     /* allocptr += total length */
	movl	IMMED(0),-4(allocptr)		     /* for fast strcmp */

	popl	tmpreg
#undef  tmpreg

	CONTINUE
2:
	movl	mlstate_ptr, temp
	movl	IMMED(REQ_ALLOC_STRING), request_w
	movl	IMMED(FUN_MASK), mask
	jmp	CSYM(set_request)

/* create_v_a : int * 'a list -> 'a vector
 *	creates a vector with elements taken from a list.
 *	n.b. The frontend ensures that list cannot be nil.
 */
ML_CODE_HDR(create_v_a)
	CHECKLIMIT(FUN_MASK)
	movl 	0(stdarg),temp               /* desired length into temp */
	sarl	IMMED(1),temp			     /* untagged */
	cmpl	IMMED(SMALL_OBJ_SZW),temp
	jge	3f

#define	tmpreg	misc1
	pushl	tmpreg

	movl	temp,tmpreg		     /* build descriptor in tmpreg */
	sall	IMMED(TAG_SHIFTW),tmpreg
	orl	IMMED(MAKE_TAG(DTAG_vector)),tmpreg
	movl	tmpreg,0(allocptr)	     /* write descriptor */
	addl	IMMED(4),allocptr
	movl	4(stdarg),tmpreg	     /* list of initial values */
	movl	allocptr,stdarg		     /* stdarg gets ptr to new array */
2:					     /* loop: */
	movl	0(tmpreg),temp		     	/* temp <- hd(tmpreg) */
	stosl				        /* 0(allocptr++) <- temp */
	movl	4(tmpreg),tmpreg	     	/* tmpreg <- tl(tmpreg) */
	cmpl	IMMED(ML_nil),tmpreg		     	/* end of list */
	jne	2b

	popl	tmpreg
#undef  tmpreg

	CONTINUE
3:
	movl	mlstate_ptr, temp
	movl	IMMED(REQ_ALLOC_VECTOR), request_w
	movl	IMMED(FUN_MASK), mask
	jmp	CSYM(set_request)
	
/* try_lock: spin_lock -> bool. 
 * low-level test-and-set style primitive for mutual-exclusion among 
 * processors.	For now, we only provide a uni-processor trivial version.
 */
ML_CODE_HDR(try_lock_a)
#if (MAX_PROCS > 1)
#  error multiple processors not supported
#else /* (MAX_PROCS == 1) */
	movl	(stdarg), temp		/* Get old value of lock. */
	movl	IMMED(1), (stdarg)		/* Set the lock to ML_false. */
	movl	temp, stdarg		/* Return old value of lock. */
	CONTINUE
#endif

/* unlock : releases a spin lock 
 */
ML_CODE_HDR(unlock_a)
#if (MAX_PROCS > 1)
#  error multiple processors not supported
#else /* (MAX_PROCS == 1) */
	movl	IMMED(3), (stdarg)		/* Store ML_true into lock. */
	movl	IMMED(1), stdarg		/* Return unit. */
	CONTINUE
#endif


/********************* Floating point functions. *********************/

#define FPOP	fstp %st	/* Pop the floating point register stack. */


/* Temporary storage for the old and new floating point control
   word.  We don't use the stack to for this, since doing so would 
   change the offsets of the pseudo-registers. */
	DATA
	ALIGN4
old_controlwd:	
	.word	0
new_controlwd:	
	.word	0
	TEXT
	ALIGN4

/*
 * Initialize the 80387 floating point coprocessor.  First, the floating
 * point control word is initialized (undefined fields are left
 * unchanged).	Rounding control is set to "nearest" (although floor_a
 * needs "toward negative infinity").  Precision control is set to
 * "double".  The precision, underflow, denormal 
 * overflow, zero divide, and invalid operation exceptions
 * are masked.  Next, seven of the eight available entries on the
 * floating point register stack are claimed (see x86/x86.sml).
 *
 * NB: this cannot trash any registers because it's called from request_fault.
 */
ENTRY(FPEEnable)
	finit
	subl	IMMED(4), %esp	/* Temp space.	Keep stack aligned. */
	fstcw	(%esp)		/* Store FP control word. */
	andw	IMMED(0xf0c0), (%esp)	/* Keep undefined fields, clear others. */
	orw	IMMED(0x023f), (%esp)	/* Set fields (see above). */
	fldcw	(%esp)		/* Install new control word. */
	addl	IMMED(4), %esp
	fldz			/* Push a zero onto the register stack. */
	fld	%st		/* Copy it 6 times. */
	fld	%st
	fld	%st
	fld	%st
	fld	%st
	fld	%st
	ret

#if (defined(OPSYS_LINUX) || defined(OPSYS_SOLARIS))
ENTRY(fegetround)
	subl	IMMED(4), %esp	/* allocate temporary space */
	fstcw	(%esp)		/* store fp control word */
	sarl	IMMED(10),(%esp)	/* rounding mode is at bit 10 and 11 */
	andl	IMMED(3), (%esp)	/* mask two bits */
	movl    (%esp),%eax	/* return rounding mode */
	addl    IMMED(4), %esp	/* deallocate space */	
	ret
  	
ENTRY(fesetround)
	subl	IMMED(4), %esp	/* allocate temporary space */	
	fstcw	(%esp)		/* store fp control word */
	andw	IMMED(0xf3ff), (%esp)	/* Clear rounding field. */
	movl    8(%esp), %eax	/* new rounding mode */
	sall	IMMED(10), %eax	/* move to right place */
	orl     %eax,(%esp)	/* new control word */
	fldcw	(%esp)		/* load new control word */
	addl	IMMED(4), %esp	/* deallocate space */
	ret
#endif

/* Save the state of the floating point unit. */
ENTRY(savefpregs)
	movl	4(%esp), temp		/* Get pointer argument. */
	fsave	(temp)
	ret

/* Restore the state of the floating point unit. */
ENTRY(restorefpregs)
	movl	4(%esp), temp		/* Arg is an ML string. */
	frstor	(temp)
	ret

/* floor : real -> int
   Return the nearest integer that is less or equal to the argument.
	 Caller's responsibility to make sure arg is in range. */

ML_CODE_HDR(floor_a)
	fstcw	old_controlwd		/* Get FP control word. */
	movw	old_controlwd, %ax
	andw	IMMED(0xf3ff), %ax		/* Clear rounding field. */
	orw	IMMED(0x0400), %ax		/* Round towards neg. infinity. */
	movw	%ax, new_controlwd
	fldcw	new_controlwd		/* Install new control word. */

	fldl	(stdarg)		/* Load argument. */
	subl	IMMED(4), %esp
	fistpl	(%esp)			/* Round, store, and pop. */
	popl	stdarg
	sall	IMMED(1), stdarg		/* Tag the resulting integer. */
	incl	stdarg

	fldcw	old_controlwd		/* Restore old FP control word. */
	CONTINUE

/* logb : real -> int
 * Extract the unbiased exponent pointed to by stdarg.
 * Note: Using fxtract, and fistl does not work for inf's and nan's.
 */
ML_CODE_HDR(logb_a)
	movl    4(stdarg),temp		/* msb for little endian arch */
	sarl	IMMED(20), temp		/* throw out 20 bits */
	andl    IMMED(0x7ff),temp		/* clear all but 11 low bits */
	subl	IMMED(1023), temp		/* unbias */
	sall    IMMED(1), temp		/* room for tag bit */
	addl	IMMED(1), temp		/* tag bit */
	movl	temp, stdarg
	CONTINUE
	

/* scalb : (real * int) -> real
 * Scale the first argument by 2 raised to the second argument.	 Raise
 * Float("underflow") or Float("overflow") as appropriate.
 * NB: We assume the first floating point "register" is
 * caller-save, so we can use it here (see x86/x86.sml). */

ML_CODE_HDR(scalb_a)
	CHECKLIMIT(FUN_MASK)
	pushl	4(stdarg)		/* Get copy of scalar. */
	sarl	IMMED(1), (%esp)		/* Untag it. */
	fildl	(%esp)			/* Load it ... */
	fstp	%st(1)			/* ... into 1st FP reg. */
	addl	IMMED(4), %esp		/* Discard copy of scalar. */

	movl	(stdarg), temp		/* Get pointer to real. */
	fldl	(temp)			/* Load it into temp. */

	fscale				/* Multiply exponent by scalar. */
	movl	IMMED(DESC_reald), (allocptr)
	fstpl	4(allocptr)		/* Store resulting float. */
	addl	IMMED(4), allocptr		/* Allocate word for tag. */
	movl	allocptr, stdarg	/* Return a pointer to the float. */
	addl	IMMED(8), allocptr		/* Allocate room for float. */
	CONTINUE

/* end of X86.prim.asm */
