/* X86.prim.asm
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This was derived from I386.prim.s, by Mark Leone (mleone@cs.cmu.edu)
 *
 * Completely rewritten and changed to use assyntax.h, by Lal George.
 */

#include "assyntax.h"
#include "ml-base.h"
#include "asm-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-request.h"
#include "ml-limits.h"
	
/*
 *
 * The 386 registers are used as follows:
 *
 * EAX - temp1 (see the code generator, x86/x86.sml)
 * EBX - misc0
 * ECX - misc1
 * EDX - misc2
 * ESI - standard continuation (ml_cont, see ml_state.h)
 * EBP - standard argument (ml_arg)
 * EDI - free space pointer (ml_allocptr)
 * ESP - stack pointer
 * EIP - program counter (ml_pc)
 */   

/* Registers (see x86/x86.sml): */
#define temp		EAX
#define misc0		EBX
#define misc1		ECX
#define misc2		EDX
#define stdcont		ESI
#define stdarg		EBP
#define allocptr	EDI
#define stackptr        ESP

/* other reg uses */
#define creturn 	EAX

	/* Stack frame */
#define tempmem		REGOFF(0,ESP)
#define baseptr		REGOFF(4,ESP)
#define exncont		REGOFF(8,ESP)
#define limitptr	REGOFF(12,ESP)
#define pc		REGOFF(16,ESP)
#define unused_1	REGOFF(20,ESP)
#define storeptr	REGOFF(24,ESP)
#define varptr		REGOFF(28,ESP)
#define start_gc	REGOFF(32,ESP)
#define unused_2	REGOFF(36,ESP)
#define eaxSpill	REGOFF(40,ESP) /* eax=0 */
#define	ecxSpill	REGOFF(44,ESP) /* ecx=1 */
#define	edxSpill	REGOFF(48,ESP) /* edx=2 */
#define	ebxSpill	REGOFF(52,ESP) /* ebx=3 */
#define	espSpill	REGOFF(56,ESP) /* esp=4 */
#define	ebpSpill	REGOFF(60,ESP) /* ebp=5 */
#define	esiSpill	REGOFF(64,ESP) /* esi=6 */
#define	ediSpill	REGOFF(68,ESP) /* edi=7 */
#define stdlink		REGOFF(72,ESP)
#define	stdclos		REGOFF(76,ESP)

#define ML_STATE_OFFSET 176
#define mlstate_ptr	REGOFF(ML_STATE_OFFSET, ESP)
#define freg8           184	     /* double word aligned */ 
#define	freg9           192
#define freg31          368          /* 152 + (31-8)*8 */
#define	fpTempMem	376	     /* freg31 + 8 */
#define SpillAreaStart	512	     /* starting offset */	
#define ML_FRAME_SIZE	(8192)

#define	via

	SEG_DATA
	ALIGNDATA4
request_w:		/* place to put the request code */
	D_LONG 0
	GLOBL CSYM(ML_X86Frame)
LABEL(CSYM(ML_X86Frame)) /* ptr to the ml frame (gives C access to limitptr) */
	D_LONG 0		

SavedSP:
	D_LONG 0		/* Value of stack pointer to restore */


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

#define cresult	EAX

#define CALLEE_SAVE	\
	PUSH_L(EBX);	\
	PUSH_L(ESI);	\
	PUSH_L(EDI);	\
	PUSH_L(EBP)	

#define CALLEE_RESTORE	\
	POP_L(EBP);	\
	POP_L(EDI);	\
	POP_L(ESI);	\
	POP_L(EBX) 

/* MOVE copies one memory location to another, using a specified temporary. */

#define MOVE(src,tmp,dest)	\
	MOV_L(src, tmp);	\
	MOV_L(tmp, dest)

#define CONTINUE				\
	JMP(CODEPTR(stdcont))

#define CHECKLIMIT				\
 1:;						\
	MOVE(stdlink, temp, pc)	;		\
	CMP_L(limitptr, allocptr);		\
	JB(9f);					\
	CALL(CSYM(saveregs));			\
	JMP(1b);				\
 9:

/**********************************************************************/
	SEG_TEXT
	ALIGNTEXT4

ML_CODE_HDR(sigh_return_a)
	MOV_L(CONST(ML_unit),stdlink)
	MOV_L(CONST(ML_unit),stdclos)
	MOV_L(CONST(ML_unit),pc)
	MOV_L(CONST(REQ_SIG_RETURN), request_w)
	JMP(CSYM(set_request))

/* sigh_resume:
 * Resume execution at the point at which a handler trap occurred.  This is a
 * standard two-argument function, thus the closure is in ml_cont.
 */

ENTRY(sigh_resume)
	MOV_L(CONST(REQ_SIG_RESUME), request_w)
	JMP(CSYM(set_request))

/* pollh_return_a:
 * The return continuation for the ML poll handler.
 */
ML_CODE_HDR(pollh_return_a)
	MOV_L(CONST(REQ_POLL_RETURN), request_w)
	MOV_L(CONST(ML_unit),stdlink)
	MOV_L(CONST(ML_unit),stdclos)
	MOV_L(CONST(ML_unit),pc)
	JMP(CSYM(set_request))

/* pollh_resume:
 * Resume execution at the point at which a poll event occurred.
 */
ENTRY(pollh_resume)
	MOV_L(CONST(REQ_POLL_RESUME), request_w)
	JMP(CSYM(set_request))

ML_CODE_HDR(handle_a)
	MOV_L(CONST(REQ_EXN), request_w)
	MOVE(stdlink,temp,pc)
	JMP(CSYM(set_request))

ML_CODE_HDR(return_a)
	MOV_L(CONST(REQ_RETURN), request_w)
	MOV_L(CONST(ML_unit),stdlink)
	MOV_L(CONST(ML_unit),stdclos)
	MOV_L(CONST(ML_unit),pc)
	JMP(CSYM(set_request))

/* Request a fault.  The floating point coprocessor must be reset
 * (thus trashing the FP registers) since we don't know whether a 
 * value has been pushed into the temporary "register".	 This is OK 
 * because no floating point registers will be live at the start of 
 * the exception handler.
 */
ENTRY(request_fault)
	CALL(CSYM(FPEEnable))          /* Doesn't trash any general regs. */
	MOV_L(CONST(REQ_FAULT), request_w)
	MOVE(stdlink,temp,pc)
	JMP(CSYM(set_request))

/* bind_cfun : (string * string) -> c_function
 */
ML_CODE_HDR(bind_cfun_a)
	CHECKLIMIT
	MOV_L(CONST(REQ_BIND_CFUN), request_w)
	JMP(CSYM(set_request))

ML_CODE_HDR(build_literals_a)
	CHECKLIMIT
	MOV_L(CONST(REQ_BUILD_LITERALS), request_w)
	JMP(CSYM(set_request))

ML_CODE_HDR(callc_a)
	CHECKLIMIT
	MOV_L(CONST(REQ_CALLC), request_w)
	JMP(CSYM(set_request))

ENTRY(saveregs)
	POP_L(pc)
	MOV_L(CONST(REQ_GC), request_w)
	/* fall into set_request */

ENTRY(set_request)
	/* temp holds mlstate_ptr, valid request in request_w  */
	/* Save registers */
	MOV_L(mlstate_ptr, temp)
	MOV_L(allocptr, REGOFF(AllocPtrOffMSP,temp))
	MOV_L(stdarg, REGOFF(StdArgOffMSP,temp))
	MOV_L(stdcont, REGOFF(StdContOffMSP,temp))

#define	temp2 allocptr
	/* note that we have left ML code */
	MOV_L(REGOFF(VProcOffMSP,temp), temp2)
	MOV_L(CONST(0), REGOFF(InMLOffVSP,temp2))

	MOV_L(misc0, REGOFF(Misc0OffMSP,temp))
	MOV_L(misc1, REGOFF(Misc1OffMSP,temp))
	MOV_L(misc2, REGOFF(Misc2OffMSP,temp))

	/* Save vregs before the stack frame is popped. */
	MOVE(limitptr,temp2, REGOFF(LimitPtrOffMSP,temp))
	MOVE(exncont, temp2, REGOFF(ExnPtrOffMSP,temp)) 
	MOVE(stdclos, temp2, REGOFF(StdClosOffMSP,temp))
	MOVE(stdlink, temp2, REGOFF(LinkRegOffMSP,temp))
	MOVE(pc, temp2, REGOFF(PCOffMSP,temp))
	MOVE(storeptr,temp2, REGOFF(StorePtrOffMSP,temp))
	MOVE(varptr,  temp2, REGOFF(VarPtrOffMSP,temp))
#undef	temp2	
	
	/* return val of function is request code */
	MOV_L(request_w,creturn)

	/* Pop the stack frame and return to run_ml(). */
	MOV_L(SavedSP, ESP)
	CALLEE_RESTORE
	RET

	SEG_TEXT
	ALIGNTEXT4
ENTRY(restoreregs)
	MOV_L(4(ESP), temp)		/* Get argument (MLState ptr). */
	CALLEE_SAVE

	MOV_L(ESP, SavedSP)		/* save stack pointer */

	/* Align on 8 byte boundary. Assumes that the stack
	 * starts out being at least word aligned. But who knows ...
	 */
	OR_L(CONST(4), ESP)		
	SUB_L(CONST(4), ESP)		/* stack grows from high to low */
	
#define temp2	EBX
	/* Allocate and initialize the ML stack frame. */
	SUB_L(CONST(ML_FRAME_SIZE), ESP)
	MOVE(REGOFF(ExnPtrOffMSP, temp),  temp2, exncont)
	MOVE(REGOFF(LimitPtrOffMSP, temp), temp2, limitptr)
	MOVE(REGOFF(StorePtrOffMSP, temp), temp2, storeptr)
	MOVE(REGOFF(VarPtrOffMSP, temp),   temp2, varptr)
	LEA_L(CSYM(saveregs), temp2)
	MOV_L(temp2,start_gc)
	MOV_L(temp, mlstate_ptr)

	/* vregs */
	MOVE	(LinkRegOffMSP(temp),  temp2, stdlink)
	MOVE	(StdClosOffMSP(temp),  temp2, stdclos)

	/* PC */
	MOVE    (PCOffMSP(temp), temp2, pc)
#undef	temp2

	/* Load ML registers. */
	MOV_L(REGOFF(AllocPtrOffMSP,temp), allocptr)
	MOV_L(REGOFF(StdContOffMSP,temp), stdcont)
	MOV_L(REGOFF(StdArgOffMSP,temp), stdarg)
	MOV_L(REGOFF(Misc0OffMSP,temp), misc0)
	MOV_L(REGOFF(Misc1OffMSP,temp), misc1)
	MOV_L(REGOFF(Misc2OffMSP,temp), misc2)

	MOV_L(ESP,CSYM(ML_X86Frame))	/* frame ptr for signal handler. */

	PUSH_L(misc2)			/* free up a register   */
	PUSH_L(temp)			/* save msp temporarily */

#define	tmpreg	misc2

	/* note that we're entering ML */
	MOV_L(REGOFF(VProcOffMSP,temp),temp)  /* temp is now vsp */
#define vsp	temp
	MOV_L(CONST(1),REGOFF(InMLOffVSP,vsp))

	/* handle signals */
	MOV_L(REGOFF(NPendingSysOffVSP,vsp),tmpreg)
	ADD_L(REGOFF(NPendingOffVSP,vsp),tmpreg)
	CMP_L(CONST(0),tmpreg)
	
#undef  tmpreg
	JNE(pending)

restore_and_jmp_ml:
	POP_L(temp)			/* restore temp to msp */
	POP_L(misc2)
	
jmp_ml:
	CMP_L(limitptr, allocptr)
	JMP(CODEPTR(REGOFF(PCOffMSP,temp)))	/* Jump to ML code. */


pending:
					/* Currently handling signal? */
	CMP_L(CONST(0), REGOFF(InSigHandlerOffVSP,vsp))   
	JNE(restore_and_jmp_ml)
					/* handler trap is now pending */
	movl	IMMED(1),HandlerPendingOffVSP(vsp) 

	/* must restore here because limitptr is on stack */ /* XXX */
	POP_L(temp)			/* restore temp to msp */
	POP_L(misc2)

	MOV_L(allocptr,limitptr)
	JMP(jmp_ml)			/* Jump to ML code. */
#undef  vsp

/* ----------------------------------------------------------------------
 * array : (int * 'a) -> 'a array
 * Allocate and initialize a new array.	 This can cause GC.
 */
ML_CODE_HDR(array_a)
	CHECKLIMIT
	MOV_L(REGIND(stdarg),temp)         /* temp := length in words */
	SAR_L(CONST(1),temp)		     /* temp := length untagged */
	CMP_L(CONST(SMALL_OBJ_SZW),temp)     /* is this a small object */
	JGE(3f)

#define temp1 misc0
#define temp2 misc1
	PUSH_L(misc0)			     /* save misc0 */ 
	PUSH_L(misc1)			     /* save misc1 */
	
	MOV_L(temp, temp1)
	SAL_L(CONST(TAG_SHIFTW),temp1)      /* build descriptor in temp1 */
	OR_L(CONST(MAKE_TAG(DTAG_arr_data)),temp1)
	MOV_L(temp1,REGIND(allocptr))	     /* store descriptor */
	ADD_L(CONST(4),allocptr)	     /* allocptr++ */
	MOV_L(allocptr, temp1)		     /* temp1 := array data ptr */
	MOV_L(REGOFF(4,stdarg), temp2)	     /* temp2 := initial value */ 
2:	
	MOV_L(temp2, REGIND(allocptr))     /* initialize array */
	ADD_L(CONST(4), allocptr)
	SUB_L(CONST(1), temp)
	JNE(2b)

	/* Allocate array header */
	MOV_L(CONST(DESC_polyarr),REGIND(allocptr)) /* descriptor in temp */
	ADD_L(CONST(4), allocptr)	     /* allocptr++ */
	MOV_L(REGIND(stdarg), temp)	     /* temp := length */
	MOV_L(allocptr, stdarg)   	     /* result = header addr */ 
	MOV_L(temp1, REGIND(allocptr))	     /* store pointer to data */
	MOV_L(temp, REGOFF(4,allocptr))	     /* store length */
	ADD_L(CONST(8), allocptr)

	POP_L(misc1)
	POP_L(misc0)
	CONTINUE
#undef  temp1
#undef  temp2
3:
	MOV_L(CONST(REQ_ALLOC_ARRAY), request_w)
	MOVE	(stdlink, temp, pc)
	JMP(CSYM(set_request))
	

/* create_r : int -> realarray */
ML_CODE_HDR(create_r_a)
	CHECKLIMIT
#define temp1 misc0
        PUSH_L(misc0)			/* free temp1 */
	MOV_L(stdarg,temp)		/* temp := length */
	SAR_L(CONST(1),temp)		/* temp := untagged length */
	SHL_L(CONST(1),temp)		/* temp := length in words */
	CMP_L(CONST(SMALL_OBJ_SZW),temp)
	JGE(2f)

	OR_L(CONST(4),allocptr)	/* align allocptr */

	/* allocate the data object */
	MOV_L(temp, temp1)
	SHL_L(CONST(TAG_SHIFTW),temp1)  /* temp1 := descriptor */
	OR_L(CONST(MAKE_TAG(DTAG_raw64)),temp1)
	MOV_L(temp1,REGIND(allocptr))	/* store descriptor */
	ADD_L(CONST(4), allocptr)	/* allocptr++ */
	MOV_L(allocptr, temp1)		/* temp1 := data object */
	SHL_L(CONST(2),temp)		/* temp := length in bytes */
	ADD_L(temp, allocptr)		/* allocptr += length */

	/* allocate the header object */
	MOV_L(CONST(DESC_real64arr),REGIND(allocptr))/* header descriptor */
	ADD_L(CONST(4), allocptr)	/* allocptr++ */
	MOV_L(temp1, REGIND(allocptr))	/* header data field */
	MOV_L(stdarg, REGOFF(4,allocptr))	/* header length field */
	MOV_L(allocptr, stdarg)		/* stdarg := header object */
	ADD_L(CONST(8), allocptr)	/* allocptr += 2 */

	POP_L(misc0)			/* restore temp1 */
	CONTINUE
2:
	POP_L(misc0)			/* restore temp1 */
	MOV_L(CONST(REQ_ALLOC_REALDARRAY), request_w)
	MOVE	(stdlink, temp, pc)
	JMP(CSYM(set_request))
#undef temp1


/* create_b : int -> bytearray */
ML_CODE_HDR(create_b_a)
	CHECKLIMIT
	MOV_L(stdarg,temp)		/* temp := length(tagged int) */
	SAR_L(CONST(1),temp)		/* temp := length(untagged) */
	ADD_L(CONST(3),temp)
	SAR_L(CONST(2),temp)		/* temp := length(words) */
	CMP_L(CONST(SMALL_OBJ_SZW),temp) /* small object? */
	JMP(2f)
	JGE(2f)				/* XXXXX */

#define	temp1	misc0
	PUSH_L(misc0)

	/* allocate teh data object */
	MOV_L(temp, temp1)		/* temp1 :=  descriptor */
	SHL_L(CONST(TAG_SHIFTW),temp1)
	OR_L(CONST(MAKE_TAG(DTAG_raw32)),temp1)
	MOV_L(temp1, REGIND(allocptr))	/* store descriptor */
	ADD_L(CONST(4), allocptr)	/* allocptr++ */
	MOV_L(allocptr, temp1)		/* temp1 := data object */
	SHL_L(CONST(2), temp)		/* temp := length in bytes */
	ADD_L(temp, allocptr)		/* allocptr += length */

	/* allocate the header object */
	MOV_L(CONST(DESC_word8arr), REGIND(allocptr))/* header descriptor */
	ADD_L(CONST(4),allocptr)	/* allocptr++ */
	MOV_L(temp1, REGIND(allocptr))	/* header data field */
	MOV_L(stdarg, REGOFF(4,allocptr))	/* header length field */
	MOV_L(allocptr, stdarg)		/* stdarg := header object */
	ADD_L(CONST(8),allocptr)	/* allocptr := 2 */
	POP_L(misc0)
	CONTINUE
#undef  temp1
2:
	MOV_L(CONST(REQ_ALLOC_BYTEARRAY), request_w)
	MOVE	(stdlink, temp, pc)
	JMP(CSYM(set_request))


/* create_s : int -> string */
ML_CODE_HDR(create_s_a)
	CHECKLIMIT
	MOV_L(stdarg,temp)
	SAR_L(CONST(1),temp)		/* temp := length(untagged) */
	ADD_L(CONST(4),temp)		
	SAR_L(CONST(2),temp)		/* temp := length(words) */
	CMP_L(CONST(SMALL_OBJ_SZW),temp)
	JGE(2f)

	PUSH_L(misc0)			/* free misc0 */
#define	temp1	misc0

	MOV_L(temp, temp1)
	SHL_L(CONST(TAG_SHIFTW),temp1)	/* build descriptor in temp1 */
	OR_L(CONST(MAKE_TAG(DTAG_raw32)), temp1)
	MOV_L(temp1, REGIND(allocptr))/* store the data pointer */
	ADD_L(CONST(4),allocptr)	/* allocptr++ */

	MOV_L(allocptr, temp1)		/* temp1 := data object */
	SHL_L(CONST(2),temp)		/* temp := length in bytes */
	ADD_L(temp, allocptr)		/* allocptr += length */
	MOV_L(CONST(0),REGOFF(-4,allocptr))	/* zero out the last word */

	/* allocate the header object */
	MOV_L(CONST(DESC_string), temp)	/* header descriptor */
	MOV_L(temp, REGIND(allocptr))
	ADD_L(CONST(4), allocptr)	/* allocptr++ */
	MOV_L(temp1, REGIND(allocptr))/* header data field */
	MOV_L(stdarg, REGOFF(4,allocptr))	/* header length field */
	MOV_L(allocptr, stdarg)		/* stdarg := header object */
	ADD_L(CONST(8), allocptr)		
	
	POP_L(misc0)			/* restore misc0 */
#undef  temp1
	CONTINUE
2:
	MOV_L(CONST(REQ_ALLOC_STRING), request_w)
	MOVE	(stdlink, temp, pc)
	JMP(CSYM(set_request))

/* create_v_a : int * 'a list -> 'a vector
 *	creates a vector with elements taken from a list.
 *	n.b. The frontend ensures that list cannot be nil.
 */
ML_CODE_HDR(create_v_a)
	CHECKLIMIT
	PUSH_L(misc0)
	PUSH_L(misc1)
#define	temp1	misc0
#define temp2   misc1	
	MOV_L(REGIND(stdarg),temp)		/* temp := length(tagged) */
	MOV_L(temp, temp1)
	SAR_L(CONST(1),temp1)		/* temp1 := length(untagged) */
	CMP_L(CONST(SMALL_OBJ_SZW),temp1)
	JGE(3f)


	SHL_L(CONST(TAG_SHIFTW),temp1)	/* build descriptor in temp1 */
	OR_L(CONST(MAKE_TAG(DTAG_vec_data)),temp1)
	MOV_L(temp1,REGIND(allocptr))	/* store descriptor */
	ADD_L(CONST(4),allocptr)	/* allocptr++ */
	MOV_L(REGOFF(4,stdarg),temp1)		/* temp1 := list */
	MOV_L(allocptr,stdarg)		/* stdarg := vector */

2:
	MOV_L(REGIND(temp1),temp2)		/* temp2 := hd(temp1) */
	MOV_L(temp2, REGIND(allocptr))	/* store word in vector */
	ADD_L(CONST(4), allocptr)	/* allocptr++ */
	MOV_L(REGOFF(4,temp1),temp1)		/* temp1 := tl(temp1) */
	CMP_L(CONST(ML_nil),temp1)	/* temp1 = nil? */
	JNE(2b)

	/* allocate header object */
	MOV_L(CONST(DESC_polyvec),temp1)/* descriptor in temp1 */
	MOV_L(temp1, REGIND(allocptr))	/* store descriptor */
	ADD_L(CONST(4),allocptr)	/* allocptr++ */
	MOV_L(stdarg, REGIND(allocptr))	/* header data field */
	MOV_L(temp, REGOFF(4,allocptr))	/* header length */
	MOV_L(allocptr, stdarg)		/* result = header object */
	ADD_L(CONST(8),allocptr)	/* allocptr += 2 */

	POP_L(misc1)
	POP_L(misc0)
	CONTINUE
3:
	POP_L(misc1)
	POP_L(misc0)
	MOV_L(CONST(REQ_ALLOC_VECTOR), request_w)
	MOVE	(stdlink, temp, pc)
	JMP(CSYM(set_request))
#undef  temp1
#undef  temp2	
	
/* try_lock: spin_lock -> bool. 
 * low-level test-and-set style primitive for mutual-exclusion among 
 * processors.	For now, we only provide a uni-processor trivial version.
 */
ML_CODE_HDR(try_lock_a)
#if (MAX_PROCS > 1)
#  error multiple processors not supported
#else /* (MAX_PROCS == 1) */
	MOV_L(REGIND(stdarg), temp)	/* Get old value of lock. */
	MOV_L(CONST(1), REGIND(stdarg))	/* Set the lock to ML_false. */
	MOV_L(temp, stdarg)		/* Return old value of lock. */
	CONTINUE
#endif

/* unlock : releases a spin lock 
 */
ML_CODE_HDR(unlock_a)
#if (MAX_PROCS > 1)
#  error multiple processors not supported
#else /* (MAX_PROCS == 1) */
	MOV_L(CONST(3), REGIND(stdarg))		/* Store ML_true into lock. */
	MOV_L(CONST(1), stdarg)		/* Return unit. */
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
	FINIT
	SUB_L(CONST(4), ESP)	/* Temp space.	Keep stack aligned. */
	FSTCW(REGIND(ESP))	/* Store FP control word. */
				/* Keep undefined fields, clear others. */
	AND_W(CONST(0xf0c0), REGIND(ESP))
	OR_W(CONST(0x023f), REGIND(ESP)) /* Set fields (see above). */
	FLDCW(REGIND(ESP))	/* Install new control word. */
	ADD_L(CONST(4), ESP)
	RET

#if (defined(OPSYS_LINUX) || defined(OPSYS_SOLARIS))
ENTRY(fegetround)
	SUB_L(CONST(4), ESP)	/* allocate temporary space */
	FSTCW(REGIND(ESP))	/* store fp control word */
	SAR_L(CONST(10),REGIND(ESP))/* rounding mode is at bit 10 and 11 */
	AND_L(CONST(3), REGIND(ESP))/* mask two bits */
	MOV_L(REGIND(ESP),EAX)	/* return rounding mode */
	ADD_L(CONST(4), ESP)	/* deallocate space */	
	RET
  	
ENTRY(fesetround)
	SUB_L(CONST(4), ESP)	/* allocate temporary space */	
	FSTCW(REGIND(ESP))	/* store fp control word */
	AND_W(CONST(0xf3ff), REGIND(ESP))	/* Clear rounding field. */
	MOV_L(REGOFF(8,ESP), EAX)	/* new rounding mode */
	SAL_L(CONST(10), EAX)	/* move to right place */
	OR_L(EAX,REGIND(ESP))	/* new control word */
	FLDCW(REGIND(ESP))	/* load new control word */
	ADD_L(CONST(4), ESP)	/* deallocate space */
	RET
#endif


/* floor : real -> int
   Return the nearest integer that is less or equal to the argument.
	 Caller's responsibility to make sure arg is in range. */

ML_CODE_HDR(floor_a)
	FSTCW(old_controlwd)		/* Get FP control word. */
	MOV_W(old_controlwd, AX)
	AND_W(CONST(0xf3ff), AX)	/* Clear rounding field. */
	OR_W(CONST(0x0400), AX)	/* Round towards neg. infinity. */
	MOV_W(AX, new_controlwd)
	FLDCW(new_controlwd)		/* Install new control word. */

	FLD_D(REGIND(stdarg))
	SUB_L(CONST(4), ESP)
	FISTP_L(REGIND(ESP))			/* Round, store, and pop. */
	POP_L(stdarg)
	SAL_L(CONST(1), stdarg)		/* Tag the resulting integer. */
	INC_L(stdarg)

	FLDCW(old_controlwd)		/* Restore old FP control word. */
	CONTINUE

/* logb : real -> int
 * Extract the unbiased exponent pointed to by stdarg.
 * Note: Using fxtract, and fistl does not work for inf's and nan's.
 */
ML_CODE_HDR(logb_a)
	MOV_L(REGOFF(4,stdarg),temp)		/* msb for little endian arch */
	SAR_L(CONST(20), temp)		/* throw out 20 bits */
	AND_L(CONST(0x7ff),temp)	/* clear all but 11 low bits */
	SUB_L(CONST(1023), temp)	/* unbias */
	SAL_L(CONST(1), temp)		/* room for tag bit */
	ADD_L(CONST(1), temp)		/* tag bit */
	MOV_L(temp, stdarg)
	CONTINUE
	

/* scalb : (real * int) -> real
 * Scale the first argument by 2 raised to the second argument.	 Raise
 * Float("underflow") or Float("overflow") as appropriate.
 * NB: We assume the first floating point "register" is
 * caller-save, so we can use it here (see x86/x86.sml). */

ML_CODE_HDR(scalb_a)
	CHECKLIMIT
	PUSH_L(REGOFF(4,stdarg))		/* Get copy of scalar. */
	SAR_L(CONST(1), REGIND(ESP))	/* Untag it. */
	FILD_L(REGIND(ESP))			/* Load it ... */
/*	fstp	%st(1) */		/* ... into 1st FP reg. */
	MOV_L(REGIND(stdarg), temp)		/* Get pointer to real. */
	FLD_D(REGIND(temp))			/* Load it into temp. */

	FSCALE				/* Multiply exponent by scalar. */
	MOV_L(CONST(DESC_reald), REGIND(allocptr))
	FSTP_D(REGOFF(4,allocptr))		/* Store resulting float. */
	ADD_L(CONST(4), allocptr)	/* Allocate word for tag. */
	MOV_L(allocptr, stdarg)		/* Return a pointer to the float. */
	ADD_L(CONST(8), allocptr)	/* Allocate room for float. */
	FSTP_D(REGIND(ESP))			
	ADD_L(CONST(4), ESP)		/* Discard copy of scalar. */
	CONTINUE

/* end of X86.prim.asm */
