/* SPARC.prim.asm
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 *
 * AUTHOR:  John Reppy
 *	    Cornell University
 *	    Ithaca, NY 14853
 *	    jhr@cs.cornell.edu
 *
 * SPARC runtime code for ML.  Registers are used as follows:
 *
 * %g7 : exception handler continuation
 * %g6 : freespace pointer
 * %g4 : heap limit pointer
 *
 * %i0 = arg
 * %i1 = continuation
 * %i2 = closure; can be ignored because contains no free vars
 * %i3 = base code pointer
 * %g1 = link register (this should be %i4!!!)
 * %i5 = var ptr
 *
 * %o0,o1,%g1-%g3,%l0-%l7,%i4 = misc. registers (contain only pointers or tagged ints)
 * %o5 = pointer temp.
 *
 * %o2,%o3 = used for args to ml_mul & ml_div, also used as temp registers
 * %o4,%o5 = temp registers
 *
 * %o6 = %sp (not used by ML)
 * %i6 = %fp (not used by ML)
 * %i7 = return address to C code (not used by ML)
 * %o7 = not used
 *
 * The root registers in the ML state vector have the following layout,
 * where roots is guaranteed to be 8-byte aligned relative to the start
 * of the ML state vector (see "ml-state.h" and "mlstate-offsets.h"):
 *
 *		  +-------------------+
 *	roots:    |   ml_arg (%i0)    |
 *		  +-------------------+
 *	roots+4:  |   ml_cont (%i1)   |
 *		  +-------------------+
 *	roots+8:  | ml_closure (%i2)  |
 *		  +-------------------+
 *      roots+12: | ml_baseReg (%i3)  |
 *		  +-------------------+
 *	roots+16: | ml_linkReg (%g1)  |  *** THIS SHOULD BE %i4 ***
 *		  +-------------------+
 *	roots+20: |  ml_varReg (%i5)  |
 *		  +-------------------+
 *	roots+24: |       ml_pc       |
 *		  +-------------------+
 *	roots+28: | ml_exncont (%g7)  |
 *		  +-------------------+
 *      roots+32: |     (%g2-%g3)     |
 *		  +-------------------+
 *	roots+40: |     (%o0-%o1)     |
 *		  +-------------------+
 *	roots+48: |     (%l0-%l7)     |
 *		  +-------------------+
 *      roots+72: |        %i4        |
 *		  +-------------------+
 */


#include "asm-base.h"
#include "ml-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-request.h"
#include "reg-mask.h"
#include "ml-limits.h"
#include "mlstate-offsets.h"	/** this file is generated **/


/* IMPORTANT NOTE:
 * Many of these registers are in "register pairs",  which is used
 * to advantage when loading and storing them from/to the ML State.
 * Beware if you re-arrange these register assignments 
 * (look for ldd and std instructions).  The same goes for the
 * offsets in the ML state vector
 */
#define      ZERO %g0
#define   EXNCONT %g7  /* exception handler (ml_exncont)	*/
#define  ALLOCPTR %g6  /* freespace pointer (ml_allocptr) */
#define  STOREPTR %g5  /* store pointer     (ml_storeptr) */
#define  LIMITPTR %g4  /* heap limit pointer (ml_limitptr)*/
#define    STDARG %i0  /* standard argument (ml_arg)  	*/
#define   STDCONT %i1  /* standard continuation (ml_cont) */
#define   STDCLOS %i2  /* standard closure  (ml_clos)	*/
#define   BASEPTR %i3  /* base code pointer (ml_roots[])  */
#define    VARPTR %i5  /* var pointer       (ml_varptr)   */
#define   STDLINK %g1  
#define  MISCREG0 %g2  /* miscellaneous registers (ml_roots[]) */
#define  MISCREG1 %g3  /* the first few of these may be callee-save */
#define  MISCREG2 %o0
#define  MISCREG3 %o1
#define  MISCREG4 %l0
#define  MISCREG5 %l1
#define  MISCREG6 %l2
#define  MISCREG7 %l3
#define  MISCREG8 %l4
#define  MISCREG9 %l5
#define MISCREG10 %l6
#define MISCREG11 %l7
#define MISCREG12 %i4
#define   TMPREG1 %o2
#define   TMPREG2 %o3
#define   TMPREG3 %o4
#define   TMPREG4 %o5	/* also used to pass register mask to g.c. */
#define    GCLINK %o7	/* link register for return from g.c.  (ml_pc) */

/* %o2 and %o3 are also used as for multiply and divide */

/*
 * %o6 = %sp (not used by ML)
 * %i6 = %fp (not used by ML)
 * %i7 = return address to C code (not used by ML)
 *
 * The ML stack frame has the following layout (set up by restoreregs):
 *
 *			+-------------------+
 *	%fp = %sp+112:	|  empty            |
 *			+-------------------+
 *	%sp+108:	|   pseudo reg 2    |
 *			+-------------------+
 *	%sp+104:	|   pseudo reg 1    |
 *			+-------------------+
 *	%sp+100:	| addr of _saveregs |
 *			+-------------------+
 *	%sp+96:	        |  ptr to MLState   |
 *			+-------------------+
 *	%sp+92:		|  temp for floor   |
 *			+-------------------+
 *	%sp+88:		|  temp for cvti2d  |
 *			+-------------------+
 *      %sp+84:		|  addr of _ml_udiv |
 *			+-------------------+
 *      %sp+80:		|  addr of _ml_umul |
 *			+-------------------+
 *	%sp+76:		|  addr of _ml_div  |
 *			+-------------------+
 *	%sp+72:		|  addr of _ml_mul  |
 *			+-------------------+
 *	%sp+68:		|     saved %g6     |
 *			+-------------------+
 *	%sp+64:		|     saved %g7     |
 *			+-------------------+
 *			|   space to save   |
 *			|   in and local    |
 *	%sp:		|     registers     |
 *			+-------------------+
 *
 * Note that this must be a multiple of 8 bytes.  The size of the
 * stack frame is:
 */
#define ML_FRAMESIZE (WINDOWSIZE+48)

#define MUL_OFFSET 72
#define DIV_OFFSET 76
#define UMUL_OFFSET 80
#define UDIV_OFFSET 84
#define FLOOR_OFFSET 92
#define MLSTATE_OFFSET 96
#define STARTGC_OFFSET 100
#define PSEUDOREG_OFFSET 104

#if (CALLEESAVE > 0)
#define CONTINUE				\
            jmp     STDCONT;			\
            subcc   ALLOCPTR,LIMITPTR,%g0
#else
#define CONTINUE				\
	    ld	    [STDCONT],STDLINK;	\
	    jmp	    STDLINK;			\
            subcc   ALLOCPTR,LIMITPTR,%g0
#endif

#define CHECKLIMIT(mask,label)			\
		bl 	label;			\
		nop;				\
		set	mask,TMPREG4;		\
 		mov	STDLINK,GCLINK;		\
		ba	CSYM(saveregs);		\
		nop;				\
	label:


	TEXT

/* sigh_return_a:
 * The return continuation for the ML signal handler.
 */
ML_CODE_HDR(sigh_return_a)
	set	RET_MASK,TMPREG4
	ba	set_request
	set	REQ_SIG_RETURN,TMPREG3	/* (delay slot) */

/* sigh_resume:
 * Resume execution at the point at which a handler trap occurred.  This is a
 * standard two-argument function, thus the closure is in ml_cont (stdcont).
 */
ENTRY(sigh_resume)
	set	FUN_MASK,TMPREG4
	ba	set_request
	set	REQ_SIG_RESUME,TMPREG3	/* (delay slot) */

/* pollh_return_a:
 * The return continuation for the ML poll handler.
 */
ML_CODE_HDR(pollh_return_a)
	set	RET_MASK,TMPREG4
	ba	set_request
	set	REQ_POLL_RETURN,TMPREG3	/* (delay slot) */

/* pollh_resume:
 * Resume execution at the point at which a poll event occurred.
 */
ENTRY(pollh_resume)
	set	FUN_MASK,TMPREG4
	ba	set_request
	set	REQ_POLL_RESUME,TMPREG3	/* (delay slot) */

ML_CODE_HDR(handle_a)
	set	EXN_MASK,TMPREG4
	ba	set_request
	set	REQ_EXN,TMPREG3		/* (delay slot) */

ML_CODE_HDR(return_a)
	set	RET_MASK,TMPREG4
	ba	set_request
	set	REQ_RETURN,TMPREG3		/* (delay slot) */

ENTRY(request_fault)
	set	EXN_MASK,TMPREG4
	ba	set_request
	set	REQ_FAULT,TMPREG3		/* (delay slot) */

/* bind_cfun : (string * string) -> c_function
 */
ML_CODE_HDR(bind_cfun_a)
	CHECKLIMIT(FUN_MASK,bind_cfun_v_limit)
	set	FUN_MASK,TMPREG4
	ba	set_request
	set	REQ_BIND_CFUN,TMPREG3		/* (delay slot) */

ML_CODE_HDR(build_literals_a)
	CHECKLIMIT(FUN_MASK,build_literals_a_limit)
	set	FUN_MASK,TMPREG4
	ba	set_request
	set	REQ_BUILD_LITERALS,TMPREG3	/* (delay slot) */

ML_CODE_HDR(callc_a)
	CHECKLIMIT(FUN_MASK,callc_a_limit)
	set	FUN_MASK,TMPREG4
	set	REQ_CALLC,TMPREG3
	/* fall through */

set_request:				/* a quick return to run_ml().  TMPREG3 holds */
					/* the request code. */
	ld	[%sp+MLSTATE_OFFSET],TMPREG2	/* get MLState ptr from stack */
	ld	[TMPREG2+VProcOffMSP],TMPREG1	/* TMPREG1 := VProcPtr */
	st	TMPREG4,[TMPREG2+MaskOffMSP]	/* save the register mask */
	st	%g0,[TMPREG1+InMLOffVSP]	/* note that we have left ML code */
	st	ALLOCPTR,[TMPREG2+AllocPtrOffMSP]
	st	STOREPTR,[TMPREG2+StorePtrOffMSP]/* save storeptr */
	st	STDLINK,[TMPREG2+LinkRegOffMSP]
	st	STDLINK,[TMPREG2+PCOffMSP]	/* PC of called function */
	std	STDARG,[TMPREG2+StdArgOffMSP]	/* save STDARG, stdcont */
	st	STDCLOS,[TMPREG2+StdClosOffMSP]	/* save closure */
	st	VARPTR,[TMPREG2+VarPtrOffMSP]	/* save varptr */
	st	EXNCONT,[TMPREG2+ExnPtrOffMSP]	/* save exncont */

#if (CALLEESAVE > 0)
        std     MISCREG0,[TMPREG2+MiscRegOffMSP(0)]
#if (CALLEESAVE > 2)
        std     MISCREG2,[TMPREG2+MiscRegOffMSP(2)]
#if (CALLEESAVE > 4)
        std     MISCREG4,[TMPREG2+MiscRegOffMSP(4)]
#if (CALLEESAVE > 6)
        std     MISCREG6,[TMPREG2+MiscRegOffMSP(6)]
#if (CALLEESAVE > 8)
        std     MISCREG8,[TMPREG2+MiscRegOffMSP(8)]
#endif
#endif
#endif
#endif
#endif
        ld      [%sp+PSEUDOREG_OFFSET],%g6         /* save pseduo registers */
        ld      [%sp+PSEUDOREG_OFFSET+4],%g7       
        st      %g6,[TMPREG2+PseudoReg1OffMSP] 
        st      %g7,[TMPREG2+PseudoReg2OffMSP] 
        ldd	[%sp+64],%g6		    /* restore C registers %g6 & %g7.*/

	mov	TMPREG3,%i0		    /* return request code */
	ret
	restore				    /* restore C register window (delay slot) */

ENTRY(saveregs)
	ld	[%sp+MLSTATE_OFFSET],TMPREG2  /* get MLState ptr from stack */
	st	TMPREG4,[TMPREG2+MaskOffMSP]    /* save register mask */
#ifdef SOFT_POLL
	/* free some regs */
	std	MISCREG0,[TMPREG2+MiscRegOffMSP(0)]  /* save miscreg0,misreg1 */
#define	p0	MISCREG0
#define	p1	MISCREG1
#define pfreq	TMPREG4
	/* check if polling enabled (poll_freq > 0) */
	set	CSYM(_PollFreq0),pfreq
	ld	[pfreq+4],pfreq
	srl	pfreq,1,pfreq			/* strip integer tag */
	tst	pfreq
	be	check_for_gc			/* go check for real gc */
	nop	/* (delay slot) */
	ld	[TMPREG2+InPollHandlerOffMSP],p0	/* if we're in the handler */
	tst	p0
	bne	reset_limit			/* ignore poll events */
	nop	/* (delay slot) */
	set	CSYM(_PollEvent0),p0		/* load contents of ref */
	ld	[p0+4],p0
	srl	p0,1,p0
	tst	p0
	be	reset_limit
	nop	/* (delay slot) */
	/* event occurred, so set ml_pollHandlerPending */
	set	1,p0
	st	p0,[TMPREG2+PollPendingOffMSP]
	ba	do_gc		/* and handle event in the C runtime */

reset_limit:				/* reset limit ptr */
	sll	pfreq,POLL_GRAIN_BITS,pfreq	/* mult by POLL_GRAIN_CPSI */
	add	pfreq,ALLOCPTR,LIMITPTR		/* overflow handled below */

check_for_gc:
	/* ensure real limit is >= limit */
	ld	[TMPREG2+RealLimitOffMSP],p0
	sub	p0,LIMITPTR,p1
	tst	p1
	bge	ok_limit
	nop	/* (delay slot) */
	mov	p0,LIMITPTR
ok_limit:
	add	p0,-4096,LIMITPTR
/*	cmp	p0,ALLOCPTR  */
	cmp 	LIMITPTR,ALLOCPTR
	ble	do_gc	  	       	/* gc *//* should be a common case */
	nop			        /* (delay slot) */
    /* since a signal also sets limitptr == allocptr to force a trap, */
    /* we need to disambiguate poll-events/signals here */
#undef  pfreq
#define vsp     TMPREG4
	ld	[TMPREG2+VProcOffMSP],vsp
	ld	[TMPREG2+PollPendingOffMSP],p0
	ld	[vsp+NPendingOffVSP],p1
	add	p0,p1,p0
	ld	[vsp+NPendingSysOffVSP],p1
	add	p0,p1,p0
	tst	p0
	bne	do_gc
	nop	/* (delay slot) */

no_gc:	/* an uneventful poll check, back to ML */
	ldd	[TMPREG2+MiscRegOffMSP(0)],MISCREG0  /* reload miscregs */
	jmp	GCLINK
	subcc	ALLOCPTR,LIMITPTR,%g0	    /* Heap limit test (delay slot) */
		
do_gc:
	st	LIMITPTR,[TMPREG2+LimitPtrOffMSP]

#undef  vsp
#undef  p0
#undef  p1
#endif /* SOFT_POLL */

	ld	[TMPREG2+VProcOffMSP],TMPREG1	/* TMPREG1 := VProcPtr */
	st	%g0,[TMPREG1+InMLOffVSP]	/* note that we have left ML code */
	st	GCLINK,[TMPREG2+PCOffMSP]	/* resume pc */
	add	BASEPTR,-4096,BASEPTR		/* adjust the base code ptr (sub 4096) */
	st	ALLOCPTR,[TMPREG2+AllocPtrOffMSP] /* save allocptr */
	st	STOREPTR,[TMPREG2+StorePtrOffMSP] /* save storeptr */
	std	STDARG,[TMPREG2+StdArgOffMSP]	/* save STDARG, stdcont */
	std	STDCLOS,[TMPREG2+StdClosOffMSP]	/* save stdclos, baseptr */
	st	VARPTR,[TMPREG2+VarPtrOffMSP]
	st	STDLINK,[TMPREG2+LinkRegOffMSP]
	st	EXNCONT,[TMPREG2+ExnPtrOffMSP]
#ifndef SOFT_POLL  /* miscreg0 & miscreg1 saved above for SOFT_POLL */
	std	MISCREG0,[TMPREG2+MiscRegOffMSP(0)]
#endif
	std	MISCREG2,[TMPREG2+MiscRegOffMSP(2)]
	std	MISCREG4,[TMPREG2+MiscRegOffMSP(4)]
	std	MISCREG6,[TMPREG2+MiscRegOffMSP(6)]
	std	MISCREG8,[TMPREG2+MiscRegOffMSP(8)]
	std	MISCREG10,[TMPREG2+MiscRegOffMSP(10)]
	st	MISCREG12,[TMPREG2+MiscRegOffMSP(12)]
        ld      [%sp+PSEUDOREG_OFFSET],%g6   /* save pseduo registers */
        ld      [%sp+PSEUDOREG_OFFSET+4],%g7
        st      %g6,[TMPREG2+PseudoReg1OffMSP] 
        st      %g7,[TMPREG2+PseudoReg2OffMSP] 
 	ldd	[%sp+64],%g6		  /* restore C registers %g6 & %g7. */
	set	REQ_GC,%i0		  /* request GC */
	ret
	restore		    /* restore C register window (delay slot) */

ENTRY(restoreregs)
	save	%sp,-SA(ML_FRAMESIZE),%sp
	st	%i0,[%sp+MLSTATE_OFFSET]	/* save MLState ptr on stack */
	set	CSYM(saveregs),TMPREG2
	st	TMPREG2,[%sp+STARTGC_OFFSET]
	mov	%i0,TMPREG2			/* transfer MLState ptr to tmpreg2 */
	std	ALLOCPTR,[%sp+64]		/* save C registers %g6 & %g7 */
	set	_ml_mul,TMPREG1			/* set pointer to ml_mul */
	st	TMPREG1,[%sp+MUL_OFFSET]
	set	_ml_div,TMPREG1			/* set pointer to ml_div */
	st	TMPREG1,[%sp+DIV_OFFSET]
	set	_ml_umul,TMPREG1		/* set pointer to ml_umul */
	st	TMPREG1,[%sp+UMUL_OFFSET]
	set	_ml_udiv,TMPREG1		/* set pointer to ml_udiv */
	st	TMPREG1,[%sp+UDIV_OFFSET]
	ld	[TMPREG2+AllocPtrOffMSP],ALLOCPTR
	ld	[TMPREG2+LimitPtrOffMSP],LIMITPTR
	ld	[TMPREG2+StorePtrOffMSP],STOREPTR
	ld	[TMPREG2+PCOffMSP],GCLINK
	ldd	[TMPREG2+StdArgOffMSP],STDARG      /* stdarg and stdcont */
	ldd	[TMPREG2+StdClosOffMSP],STDCLOS     /* stdclos and baseptr */
	ld 	[TMPREG2+VarPtrOffMSP],VARPTR
	ld	[TMPREG2+LinkRegOffMSP],STDLINK
	ld	[TMPREG2+ExnPtrOffMSP],EXNCONT	/* restore exnptr */
        ld      [TMPREG2+PseudoReg1OffMSP],MISCREG2 /* save pseduo registers */
        ld      [TMPREG2+PseudoReg2OffMSP],MISCREG3
        st      MISCREG2,[%sp+PSEUDOREG_OFFSET]
        st      MISCREG3,[%sp+PSEUDOREG_OFFSET+4]
	ldd	[TMPREG2+MiscRegOffMSP(0)],MISCREG0
	ldd	[TMPREG2+MiscRegOffMSP(2)],MISCREG2
	ldd	[TMPREG2+MiscRegOffMSP(4)],MISCREG4
	ldd	[TMPREG2+MiscRegOffMSP(6)],MISCREG6
	ldd	[TMPREG2+MiscRegOffMSP(8)],MISCREG8
	ldd	[TMPREG2+MiscRegOffMSP(10)],MISCREG10
	ld	[TMPREG2+MiscRegOffMSP(12)],MISCREG12

	sub	BASEPTR,-4096,BASEPTR	/* adjust the base code ptr (add 4096) */
	ld	[TMPREG2+VProcOffMSP],TMPREG1	/* TMPREG1 := VProcPtr */
	set	1,TMPREG2			/* note that we have entered ML code */
	st	TMPREG2,[TMPREG1+InMLOffVSP]
	ld	[TMPREG1+NPendingSysOffVSP],TMPREG2	/* check for pending signals */
	ld	[TMPREG1+NPendingOffVSP],TMPREG3
	addcc	TMPREG2,TMPREG3,%g0
	bne	pending_sigs
	nop
CSYM(ml_go):					/* invoke the ML code */
	jmp	GCLINK
	subcc	ALLOCPTR,LIMITPTR,%g0	    /* Heap limit test (delay slot) */

pending_sigs:	/* there are pending signals */
					/* check if we are currently handling a signal */
	ld	[TMPREG1+InSigHandlerOffVSP],TMPREG2
	tst	TMPREG2
	bne	CSYM(ml_go)
	set	1,TMPREG2			    /* (delay slot) */
					/* note that a handler trap is pending */
	st	TMPREG2,[TMPREG1+HandlerPendingOffVSP]
	ba	CSYM(ml_go)
	mov	ALLOCPTR,LIMITPTR	    /* (delay slot) */


#if defined(OPSYS_SUNOS) || defined(OPSYS_NEXTSTEP)
/* ZeroLimitPtr:
 *
 * Zero the heap limit pointer so that a trap will be generated on the next limit
 * check and then continue executing ML code.
 * NOTE: this code cannot trash any registers (other than limitptr) or the condition
 * code. To achieve this we work inside a new register window.
 * Also note that this code is not needed under SOLARIS 2.x, since we can
 * directly change the register from C.
 */
	TEXT
ENTRY(ZeroLimitPtr)
	save	%sp,-SA(WINDOWSIZE),%sp
	sethi	%hi(CSYM(SavedPC)),%l1
	ld	[%l1+%lo(CSYM(SavedPC))],%o0
	set	0,LIMITPTR
	jmp	%o0
	restore				/* (delay slot) */
#endif /* OPSYS_SUNOS */


/* array : (int * 'a) -> 'a array
 * Allocate and initialize a new array.	 This can cause GC.
 */
ML_CODE_HDR(array_a)
	CHECKLIMIT(FUN_MASK,array_a_limit)
	ld	[STDARG],TMPREG1		    /* tmp1 = length in words */
	sra	TMPREG1,1,TMPREG1		    /* convert to sparc int */
	cmp	TMPREG1,SMALL_OBJ_SZW	    /* is this a small object? */
	bgt	3f
	nop
	ld	[STDARG+4],TMPREG3	    /* tmp3 = initial value */
	sll	TMPREG1,TAG_SHIFTW,TMPREG2	    /* build descriptor in TMPREG2 */
	or	TMPREG2,MAKE_TAG(DTAG_array),TMPREG2
	st	TMPREG2,[ALLOCPTR]	    /* store the descriptor */
	inc	4,ALLOCPTR		    /* allocptr++ */
	mov	ALLOCPTR,STDARG	    /* result = object addr. */
1:
	st	TMPREG3,[ALLOCPTR]
	deccc	1,TMPREG1			    /* if (--length > 0) */
	bgt	1b				/* then continue */
	inc	4,ALLOCPTR		    /* allocptr++ (delay slot) */
	/* end loop */
	CONTINUE

3:	/* here we do off-line allocation for big arrays */
	set	FUN_MASK,TMPREG4
	ba	set_request
	set	REQ_ALLOC_ARRAY,TMPREG3	    /* (delayslot) */

/* create_r : int -> realarray
 * Create a new realarray.
 */
ML_CODE_HDR(create_r_a)
	CHECKLIMIT(FUN_MASK,create_r_a_limit)
	sra	STDARG,1,TMPREG2		    /* tmp2 = length (untagged int) */
	sll	TMPREG2,2,TMPREG3		    /* tmpreg3 = length in words */
	cmp	TMPREG3,SMALL_OBJ_SZW	    /* is this a small object? */
	bgt	1f
	nop
	sll	TMPREG2,TAG_SHIFTW,TMPREG1	    /* build descriptor in tmpreg1 */
	or	TMPREG1,MAKE_TAG(DTAG_realdarray),TMPREG1
#ifdef ALIGN_REALDS
	or	ALLOCPTR,0x4,ALLOCPTR	    /* desc is unaliged */
#endif
	st	TMPREG1,[ALLOCPTR]	    /* store the descriptor */
	inc	4,ALLOCPTR		    /* allocptr++ */
	mov	ALLOCPTR,STDARG	    /* stdarg = realarray */
	sll	TMPREG3,2,TMPREG3		    /* tmpreg3 = length (in bytes) */
	add	ALLOCPTR,TMPREG3,ALLOCPTR   /* ALLOCPTR += length */
	CONTINUE

1:	/* off-line allocation of big realarrays */
	set	FUN_MASK,TMPREG4
	ba	set_request
	set	REQ_ALLOC_REALDARRAY,TMPREG3	/* (delayslot) */

/* create_b : int -> bytearray
 * Create a bytearray of the given length.
 */
ML_CODE_HDR(create_b_a)
	CHECKLIMIT(FUN_MASK,create_b_a_limit)
	sra	STDARG,1,TMPREG2		    /* tmpreg2 = length (sparc int) */
	add	TMPREG2,3,TMPREG3		    /* tmpreg3 = length in words */
	sra	TMPREG3,2,TMPREG3
	cmp	TMPREG3,SMALL_OBJ_SZW	    /* is this a small object? */
	bgt	1f
	nop
	sll	TMPREG2,TAG_SHIFTW,TMPREG1	    /* tmpreg1 is descriptor */
	or	TMPREG1,MAKE_TAG(DTAG_bytearray),TMPREG1
	st	TMPREG1,[ALLOCPTR]	    /* store the tag */
	inc	4,ALLOCPTR		    /* allocptr++ */
	sll	TMPREG3,2,TMPREG2		    /* tmpreg2 = length in bytes (no tag) */
	mov	ALLOCPTR,STDARG	    /* result = object addr */
	add	ALLOCPTR,TMPREG2,ALLOCPTR   /* allocptr += length */
	CONTINUE

1:	/* here we do off-line allocation for big bytearrays */
	set	FUN_MASK,TMPREG4
	ba	set_request
	set	REQ_ALLOC_BYTEARRAY,TMPREG3	/* (delayslot) */

/* create_s : int -> string
 * Create a string of the given length.
 */
ML_CODE_HDR(create_s_a)
	CHECKLIMIT(FUN_MASK,create_s_a_limit)
	sra	STDARG,1,TMPREG2		    /* tmpreg2 = length (sparc int) */
	add	TMPREG2,4,TMPREG3		    /* tmpreg3 = length in words */
	sra	TMPREG3,2,TMPREG3
	cmp	TMPREG3,SMALL_OBJ_SZW	    /* is this a small object? */
	bgt	1f
	nop
	sll	TMPREG2,TAG_SHIFTW,TMPREG1	    /* tmpreg1 is descriptor */
	or	TMPREG1,MAKE_TAG(DTAG_string),TMPREG1
	st	TMPREG1,[ALLOCPTR]	    /* store the tag */
	inc	4,ALLOCPTR		    /* allocptr++ */
	sll	TMPREG3,2,TMPREG2		    /* tmpreg2 = length in bytes (no tag) */
	mov	ALLOCPTR,STDARG	    /* result = object addr */
	add	ALLOCPTR,TMPREG2,ALLOCPTR   /* allocptr += length */
	st	ZERO,[ALLOCPTR-4]	    /* store 0 in last word */
	CONTINUE

1:	/* here we do off-line allocation for big strings */
	set	FUN_MASK,TMPREG4
	ba	set_request
	set	REQ_ALLOC_STRING,TMPREG3	/* (delayslot) */

/* create_v_a : (int * 'a list) -> 'a vector
 * Create a vector with elements taken from a list.
 * NOTE: the front-end ensures that list cannot be nil.
 */
ML_CODE_HDR(create_v_a)
	CHECKLIMIT(FUN_MASK,create_v_a_limit)
	ld	[STDARG],TMPREG1		    /* tmpreg1 = length (tagged int) */
	sra	TMPREG1,1,TMPREG1		    /* tmpreg1 = length (untagged int) */
	cmp	TMPREG1,SMALL_OBJ_SZW	    /* is this a small object? */
	bgt	1f
	nop
	sll	TMPREG1,TAG_SHIFTW,TMPREG2	    /* build descriptor in tmpreg2 */
	or	TMPREG2,MAKE_TAG(DTAG_vector),TMPREG2
	st	TMPREG2,[ALLOCPTR]	    /* store descriptor */
	inc	4,ALLOCPTR		    /* allocptr++ */
	ld	[STDARG+4],TMPREG2	    /* tmpreg2 := list */
	mov	ALLOCPTR,STDARG	    /* stdarg := vector */
2:					    /* loop */
	ld	[TMPREG2],TMPREG1			/* tmpreg1 = hd(tmpreg2) */
	ld	[TMPREG2+4],TMPREG2			/* tmpreg2 = tl(tmpreg2) */
	st	TMPREG1,[ALLOCPTR]		/* store element */
	cmp	TMPREG2,ML_nil			/* if (tmpreg2 <> nil) goto loop */
	bne	2b
	inc	4,ALLOCPTR			/* allocptr++ (delay slot) */
					    /* end loop */
	CONTINUE

1:	/* off-line allocation of big vectors */
	set	FUN_MASK,TMPREG4
	ba	set_request
	set	REQ_ALLOC_VECTOR,TMPREG3	/* (delayslot) */


/* floor : real -> int
 * Return the floor of the argument ; do not check for out-of-range (it's
 * the ML code's responsibility to check before calling. */
ML_CODE_HDR(floor_a)
	ld	[STDARG],%f0	    /* fetch arg into %f0, %f1. */
	ld	[STDARG+4],%f1
	ld	[STDARG],TMPREG2	    /* tmpreg2 gets high word. */
	tst	TMPREG2		    /* negative ? */
	blt	1f
	nop
				/* handle positive case */
	fdtoi	%f0,%f2		    /* cvt to int (round towards 0) */
	st	%f2,[%sp+FLOOR_OFFSET]
	ld	[%sp+FLOOR_OFFSET],TMPREG2    /* tmpreg2 gets int result (via stack temp). */
	add	TMPREG2,TMPREG2,TMPREG2
	add	TMPREG2,1,STDARG
	CONTINUE
	
1:				/* handle negative case. */
	fdtoi	%f0,%f2		    /* cvt to int (round towards 0) */
	st	%f2,[%sp+FLOOR_OFFSET]
	fitod	%f2,%f4		    /* cvt back to real to check for fraction */
	fcmpd	%f0,%f4		    /* same value? */
	ld	[%sp+FLOOR_OFFSET],TMPREG2	    /* tmpreg2 gets int result (via stack temp). */
	fbe	2f		    /* check result of fcmpd */
	nop
	dec	TMPREG2		    /* push one lower */
2:				/* cvt result to ML int, and continue */
	add	TMPREG2,TMPREG2,TMPREG2
	add	TMPREG2,1,STDARG
	CONTINUE

/* logb : real -> int
 * Extract and unbias the exponent.
 * The IEEE bias is 1023.
 */
ML_CODE_HDR(logb_a)
	ld	[STDARG],TMPREG2		/* extract exponent. */
	srl	TMPREG2,19,TMPREG2
	and	TMPREG2,2047*2,TMPREG2		/* unbias and cvt to ML int. */
	sub	TMPREG2,2045,STDARG	  	/* 2(n-1023)+1 == 2n-2045. */
	CONTINUE


/* scalb : (real * int) -> real
 * Scale the first argument by 2 raised to the second argument.	 Raise
 * Float("underflow") or Float("overflow") as appropriate.
 */
ML_CODE_HDR(scalb_a)
	CHECKLIMIT(FUN_MASK,scalb_a_limit)
	ld	[STDARG+4],TMPREG1   /* tmpreg1 gets scale (second arg) */
	sra	TMPREG1,1,TMPREG1	    /* cvt scale to sparc int */
	ld	[STDARG],STDARG   /* stdarg gets real (first arg) */
	ld	[STDARG],TMPREG4	    /* tmpreg4 gets high word of real value. */
	set	0x7ff00000,TMPREG2    /* tmpreg2 gets exponent mask. */
	andcc	TMPREG4,TMPREG2,TMPREG3   /* extract exponent into tmpreg3. */
	be	1f		    /* if 0 then return same */
	nop
	srl	TMPREG3,20,TMPREG3	    /* cvt exp to int (delay slot). */
	addcc	TMPREG3,TMPREG1,TMPREG1	    /* tmpreg1 = exp + scale */
	ble	under		    /* if new exp <= 0 then underflow */
	nop
	cmp	TMPREG1,2047	    /* if new exp >= 2047 then overflow */
	bge	over
	nop
	andn	TMPREG4,TMPREG2,TMPREG4   /* mask out old exponent. */
	sll	TMPREG1,20,TMPREG1	    /* shift new exp to exponent position. */
	or	TMPREG4,TMPREG1,TMPREG4   /* set new exponent. */
	ld	[STDARG+4],TMPREG1   /* tmpreg1 gets low word of real value. */
7:
#ifdef ALIGN_REALDS
	or	ALLOCPTR,0x4,ALLOCPTR	    /* desc is unaliged */
#endif
	st	TMPREG4,[ALLOCPTR+4] /* allocate the new real value */
	st	TMPREG1,[ALLOCPTR+8]
	set	DESC_reald,TMPREG1
	st	TMPREG1,[ALLOCPTR]
	add	ALLOCPTR,4,STDARG /* set result. */
	inc	12,ALLOCPTR	    /* allocptr += 3 */
1:	CONTINUE

over:				/* handle overflow */
	t	ST_INT_OVERFLOW	    /* generate an Overflow exn.  We do this */
	/* never get here */	    /* via a trap to produce a SIGOVFL */

under:				/* handle underflow */
	set	0,TMPREG4
	set	0,TMPREG1
	ba	7b
	nop

/** Integer multiplication and division routines **/
	.global .mul, .div, .umul, .udiv

/* ml_mul:
 * multiply %o2 by %o3, returning the result in %o2
 * Note: this code assumes that .mul doesn't trash any global or input
 * registers.
 */
_ml_mul:
	save	%sp,-SA(WINDOWSIZE),%sp
/** NOTE: if %g1, %g2, %g3 are not callee save, then this can be avoided **/
/** NOTE: .mul doesn't use %g2, %g3, but the dynamic linking initialization
 ** does.
 **/
	mov	%g1,%l1			  /* save %g1 which may get trashed */
	mov	%g2,%l2
	mov	%g3,%l3
	mov	%i2,%o0
	call	.mul
	mov	%i3,%o1			  /* (delay slot) */
	mov	%l1,%g1			  /* restore %g1 */
	mov	%l2,%g2
	mov	%l3,%g3
	bnz	1f			  /* if z is clear, then overflow */
	restore %o0,0,%o2		  /* result in %o2 (delay slot) */
	retl
	nop
1:					/* handle overflow. */
	t	ST_INT_OVERFLOW		  /* generate an Overflow exn.  We do this */
					  /* via a trap to produce a SIGOVFL */

/* ml_div:
 * divide %o2 by %o3, returning the result in %o2.
 * Note: .div uses %g1, %g2 and %g3, so we must save them.  We do this using the
 * locals of the new window, since .div is a leaf routine.
 */
_ml_div:
	save	%sp,-SA(WINDOWSIZE),%sp
	addcc	%i3,%g0,%o1		/* %o1 is divisor (and check for zero) */
	bz	1f
				    /* save %g1, %g2 and %g3 (using new window) */
/** NOTE: if %g1, %g2, %g3 are not callee save, then this can be avoided **/
	mov	%g1,%l1			/* (delay slot) */
	mov	%g2,%l2
	mov	%g3,%l3
	call	.div
	mov	%i2,%o0			/* (delay slot) */
				    /* restore %g1, %g2 and %g3 */
	mov	%l3,%g3
	mov	%l2,%g2
	mov	%l1,%g1
	ret
	restore %o0,0,%o2		/* result in %o2 (delay slot) */
1:				    /* handle zero divide */
	restore				/* restore ML window */
	t	ST_DIV0			/* generate a Div exn.  We do this via a */
					/* trap to produce a SIGDIV */

/* ml_umul:
 * multiply %o2 by %o3 (unsigned), returning the result in %o2.  This does
 * raise Overflow.
 * Note: this code assumes that .mul doesn't trash any global or input
 * registers.
 */
_ml_umul:
	save	%sp,-SA(WINDOWSIZE),%sp
/** NOTE: if %g1, %g2, %g3 are not callee save, then this can be avoided **/
/** NOTE: .mul doesn't use %g2, %g3, but the dynamic linking initialization
 ** does.
 **/
	mov	%g1,%l1			  /* save %g1 which may get trashed */
	mov	%g2,%l2
	mov	%g3,%l3
	mov	%i2,%o0
	call	.umul
	mov	%i3,%o1			  /* (delay slot) */
	mov	%l1,%g1			  /* restore %g1 */
	mov	%l2,%g2
	mov	%l3,%g3
	ret
	restore %o0,0,%o2		  /* result in %o2 (delay slot) */


/* ml_udiv:
 * divide %o2 by %o3 (unsigned), returning the result in %o2.
 * Note: .udiv uses %g1, %g2 and %g3, so we must save them.  We do this using the
 * locals of the new window, since .div is a leaf routine.
 */
_ml_udiv:
	save	%sp,-SA(WINDOWSIZE),%sp
	addcc	%i3,%g0,%o1		/* %o1 is divisor (and check for zero) */
	bz	1f
				    /* save %g1, %g2 and %g3 (using new window) */
/** NOTE: if %g1, %g2, %g3 are not callee save, then this can be avoided **/
	mov	%g1,%l1			/* (delay slot) */
	mov	%g2,%l2
	mov	%g3,%l3
	call	.udiv
	mov	%i2,%o0			/* (delay slot) */
				    /* restore %g1, %g2 and %g3 */
	mov	%l3,%g3
	mov	%l2,%g2
	mov	%l1,%g1
	ret
	restore %o0,0,%o2		/* result in %o2 (delay slot) */
1:				    /* handle zero divide */
	restore				/* restore ML window */
	t	ST_DIV0			/* generate a Div exn.  We do this via a */
					/* trap to produce a SIGDIV */


/* try_lock : spin_lock -> bool
 * low-level test-and-set style primitive for mutual-exclusion among 
 * processors.
 */
ML_CODE_HDR(try_lock_a)
#if (MAX_PROCS > 1)
	???
#else (MAX_PROCS == 1)
	ld	[STDARG],TMPREG1	/* load previous value into tmpreg1 */
	set	ML_false,TMPREG2	/* ML_false */
	st	TMPREG2,[STDARG]	/* store ML_false into the lock */
	mov	TMPREG1,STDARG		/* return previous value of lock */
	CONTINUE
#endif

/* unlock : releases a spin lock 
 */
ML_CODE_HDR(unlock_a)
#if (MAX_PROCS > 1)
	???
#else (MAX_PROCS == 1)
	set	ML_true,TMPREG1		/* store ML_true ... */
	st	TMPREG1,[STDARG]		/* into the lock */
	set	ML_unit,STDARG		/* return unit */
	CONTINUE
#endif


/* SetFSR:
 * Load the floating-point status register with the given word.
 */
ENTRY(SetFSR)
	set	fsrtmp,%o1
	st	%o0,[%o1]
	retl
	ld	[%o1],%fsr		/* (delay slot) */
	DATA
fsrtmp:	.word	0
	TEXT


/* void FlushICache (char *addr, int nbytes)
 */
ENTRY(FlushICache)
	and	%o1,0x1F,%o2	/* m <- (nbytes % (32-1)) >> 2 (use %o2 for m) */
	srl	%o2,2,%o2
	srl	%o1,5,%o1	/* i <- (nbytes >> 5) */
/* FLUSH4 implements: if (m > 0) { FLUSH addr; addr += 4; m--;} else goto L_test */
#define FLUSH4					\
		tst	%o2;			\
		ble	L_test;			\
		nop;				\
		iflush	%o0;			\
		inc	4,%o0;			\
		dec	1,%o2
	FLUSH4
	FLUSH4
	FLUSH4
	FLUSH4
	FLUSH4
	FLUSH4
	FLUSH4
				/* addr is 32-byte aligned here */
L_test:
	tst	%o1
	be	L_exit
	nop
L_loop:				/* flush 32 bytes per iteration */
	iflush	%o0
	iflush	%o0+8
	iflush	%o0+16
	iflush	%o0+24
	deccc	1,%o1		/* if (--i > 0) goto L_loop */
	bg	L_loop
	inc	32,%o0		/* addr += 32 (delay slot) */
L_exit:
	retl
	nop
