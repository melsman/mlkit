/* M68.prim.asm
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * M68 primitive runtime code for SML/NJ.
 *
 * The m680x0 registers are used as follows; the names in parentheses are the
 * MLState field names (see ml_state.h):
 *
 * a6 = freespace pointer (ml_allocptr)
 * d5 = data limit (ml_limitptr)
 * d6 = store ptr (ml_storeptr)
 * d3 = var ptr (ml_varptr)
 *
 * a0 = standard arg (ml_arg)
 * a1 = standard continuation (ml_cont)
 * a2 = standard closure (ml_closure)
 * a3 = standard link
 * d7 = exception handler continuation (ml_exncont)
 *
 * a4 = general purpose pointers (ml_roots[4] and ml_roots[5])
 * a5 = pointer temps
 * d0,d1 = arithtemps used by generic.sml
 * d2,d4 = arithtemps used by m68.sml
 *
 * a7 = stack ptr (not used by ML)
 *
 *
 * The ML state structure has the following layout for the mc680x0 (see "ml_state.h"):
 *
 *		+-------------------+
 *  MLState --> | ml_allocptr (a6)  |
 *		+-------------------+
 *	+4:	| ml_limitptr (d5)  |
 *		+-------------------+
 *	+8:	| ml_storeptr (d6)  |
 *		+-------------------+
 *	+12:	|  ml_exncont (d7)  |
 *		+-------------------+
 *	+16:	|   ml_arg (a0)     |
 *		+-------------------+
 *	+20:	|   ml_cont (a1)    |
 *		+-------------------+
 *	+24:	| ml_closure (a2)   |
 *		+-------------------+
 *	+28:	|     (a3, a4)      |
 *		+-------------------+
 *	+36:	| ml_varptr (d3)    |
 *		+-------------------+
 *	+40:	|       ml_pc       |
 *		+-------------------+
 *      +44:    |       inML        |
 *		+-------------------+
 *      +48:    |     request       |
 *		+-------------------+
 *      +52:    |   handlerPending  |
 *		+-------------------+
 *      +56:    |    inSigHandler   |
 *		+-------------------+
 *      +60:    |     maskSignals   |
 *		+-------------------+
 *      +64:    |   NumPendingSigs  |
 *		+-------------------+
 *      +68:    |     ioWaitFlag    |
 *		+-------------------+
 *      +72:    |     GCpending     |
 *		+-------------------+
 *      +76:    |      saved_pc     |
 *		+-------------------+
 *              |        ...        |
 */

#include "mask.h"
#include "tags.h"
#include "request.h"
#include "fpregs.h"

#ifdef AUX
#define CSYM(sym) sym
#define d0	%D0
#define d1	%D1
#define d2	%D2
#define d3	%D3
#define d4	%D4
#define d5	%D5
#define d6	%D6
#define d7	%D7
#define a0	%A0
#define a1	%A1
#define a2	%A2
#define a3	%A3
#define a4	%A4
#define a5	%A5
#define a6	%A6
#define sp	%SP
#define cc	%CC
#define fp0	%FP0
#define fp1	%FP1
#define fp2	%FP2
#define fp3	%FP3
#define fp4	%FP4
#define fp5	%FP5
#define fp6	%FP6
#define fp7	%FP7
#define fpcr	%FPCR
#else
#ifdef __STDC__
#define CSYM(sym) _##sym
#else
#define CSYM(sym) _/**/sym
#endif
#endif

#define allocptr 	a6
#define limit		d5
#define storeptr	d6
#define exncont		d7
#define stdarg		a0
#define stdcont		a1
#define stdclos		a2
#define stdlink		a3

#define ptrtmp		a5

#define ml_allocptr_offset 0

#define inML 44
#define request 48
#define handlerPending 52
#define inSigHandler 56
#define maskSignals 60
#define NumPendingSigs 64
#define ioWaitFlag 68
#define GCpending 72
#define saved_pc_ 76
#define mask_ 96

/* the save mask for saving C registers on callee save systems */
#define	CREGMASK	d2-d7/a2-a6
/* the MLState pointer is passed to restoreregs on the stack.  Once we
 * push the C-registers (11 of them) onto the stack, the MLState pointer
 * can be found at the following offset:
 */
/* #define MLSTATE_OFFSET ((11+1)*4) Avoid HPUX problem */
#define MLSTATE_OFFSET 48

#define ML_CODE_HDR(name)			\
	    .globl CSYM(name);			\
	    .align  2;				\
	    .word   TAG_backptr;		\
    CSYM(name):

#define CHECKLIMIT(mask)				\
	    1: jgt	2f;				\
	       lea	1b,a5;				\
	       movl	mask,d4;			\
	       rts;					\
	    2:

#define CONTINUE					\
	    movl	a1@,stdlink;			\
	    cmpl	a6,d5;				\
	    jmp 	stdlink@
	    

#define RAISE						\
	    movl	d7,a1; CONTINUE

	.text
	.globl	CSYM(sigh_resume), CSYM(saveregs), CSYM(restoreregs)

/* sigh_return_c:
 * The return continuation for the ML signal handler.
 */
ML_CODE_HDR(sigh_return_a)
	movl	sp@+,a5		/* flush startgc from stack */
	movl	sp@(MLSTATE_OFFSET),a5	/* get the MLState ptr off the stack */
	movl	#REQ_SIG_RETURN,a5@(request)
	jra	CSYM(quicksave)

/* sigh_resume:
 * Resume execution at the point at which a handler trap occurred.  This is a
 * standard two-argument function, thus the closure is in ml_cont (a1).
 */
CSYM(sigh_resume):
	movl	sp@+,a5		/* flush startgc from stack */
	movl	sp@(MLSTATE_OFFSET),a5	/* get the MLState ptr off the stack */
	movl	#REQ_SIG_RESUME,a5@(request)
	jra	CSYM(quicksave)

ML_CODE_HDR(handle_a)
	movl	sp@+,a5		/* flush startgc from stack */
	movl	sp@(MLSTATE_OFFSET),a5	/* get the MLState ptr off the stack */
	movl	#REQ_EXN,a5@(request)
	jra	CSYM(quicksave)

ML_CODE_HDR(return_a)
	movl	sp@+,a5		/* flush startgc from stack */
	movl	sp@(MLSTATE_OFFSET),a5	/* get the MLState ptr off the stack */
	movl	#REQ_RETURN,a5@(request)
	jra	CSYM(quicksave)

	.globl	CSYM(request_fault)
CSYM(request_fault):
	movl	sp@+,a5		/* flush startgc from stack */
	movl	sp@(MLSTATE_OFFSET),a5	/* get the MLState ptr off the stack */
	movl	#REQ_FAULT,a5@(request)
	jra	CSYM(quicksave)

ML_CODE_HDR(callc_a)
	CHECKLIMIT(#closmask)
	movl	sp@+,a5		/* flush startgc from stack */
	movl	sp@(MLSTATE_OFFSET),a5	/* get the MLState ptr off the stack */
	movl	#REQ_CALLC,a5@(request)
	/* fall into quicksave */

CSYM(quicksave):
	clrl	a5@(inML)		/* note that we have left ML code */
	movl	a6,a5@
	moveml	d6-d7/a0-a2,a5@(8)	/* save storeptr, exncont, arg, cont, closure */
	movl    d3,a5@(36)		/* save varptr */
	moveml	sp@+,CREGMASK		/* restore the C registers */
	rts				/* return to run_ml() */

CSYM(saveregs):
	pea	a5@
	movl	sp@(MLSTATE_OFFSET+4),a5 /* get MLState ptr off the stack */
	movl	sp@+,a5@(40)		/* save resume pc */
	clrl	a5@(inML)		/* note that we have left ML code */
	tstl	d5			/* see if it was a signal */
	bne	1f
	movl	#REQ_SIGNAL,a5@(request)
1:
	movl	d4,a5@(mask_)
	movl	a6,a5@
	moveml	d6-d7/a0-a4,a5@(8)
	movl    d3,a5@(36)		/* save varptr */
	moveml	sp@+,CREGMASK		/* restore the C registers */
	rts				/* return to run_ml() */

CSYM(restoreregs):
	moveml	CREGMASK,sp@-		/* save the C registers */
	movl	sp@(MLSTATE_OFFSET),a5  /* get the MLState ptr off the stack */
	movl	a5@,a6
	moveml	a5@(4),d5-d7/a0-a4  
	movl    a5@(36),d3
	addql	#1,a5@(inML)		/* note that we are executing ML code */
	tstl	a5@(GCpending)
	jne	3f

	tstl	a5@(NumPendingSigs)	/* check for pending signals */
	jne	2f
1:
	pea	CSYM(saveregs)
	movl	a5@(40),a5		/* fetch the saved pc */
	cmpl	a6,d5
	jmp	a5@
2:				      	/* there are pending signals */
	tstl	a5@(maskSignals)	/* are signals masked? */
	jne	1b
	tstl	a5@(inSigHandler)	/* are currently handling a signal? */
	jne	1b
	addql	#1,a5@(handlerPending)	/* note that a trap is pending */
3:	clrl	d5			/* trap on the next limit check */
	jra	1b


	.globl CSYM(savefpregs)
CSYM(savefpregs):
	link 	a6, #-4				/* temp. space */
	movl 	a0, a6@(-4)			/* save a0 */
#if (MAX_PROCS > 1)
	???
#else /* (MAX_PROCS == 1) */
	movl	CSYM(MLproc), a0		/* ml state vector */
#endif
	movl	a0@(ml_allocptr_offset), a0	/* next available heap addr */
	movl 	#MAKE_DESC(NSAVED_FPREGS*8,TAG_string), a0@+  
	fmoved	fp2, a0@+
	fmoved	fp3, a0@+
	fmoved	fp4, a0@+
	fmoved	fp5, a0@+
	fmoved	fp6, a0@+
	fmoved	fp7, a0@+
	movl	a6@(-4), a0			/* restore a0 */
	unlk	a6
	rts

	.globl  CSYM(restorefpregs)
CSYM(restorefpregs):
	link	a6, #-4				/* temp. space */
	movl	a0, a6@(-4)			/* save a0 */
	movl	a6@(8), a0			/* pointer to string of floats */
	fmoved	a0@+, fp2
	fmoved	a0@+, fp3
	fmoved	a0@+, fp4
	fmoved	a0@+, fp5
	fmoved	a0@+, fp6
	fmoved	a0@+, fp7
	movl	a6@(-4), a0			/* restore a0 */
	unlk	a6
	rts


/* adjust_limit:
 * Adjust the heap limit pointer so that a trap will be generated on the next limit
 * check and then continue executing ML code.
 * NOTE: this code cannot trash any registers  (other than d5) or the condition
 * codes.  Also, we need to come up with a solution for the case when
 * MAX_PROCS > 1 for getting at the saved_pc and condition codes.
 */
	.globl	CSYM(adjust_limit), CSYM(saved_pc)
CSYM(adjust_limit):
#if (MAX_PROCS > 1)
	???
#else /* (MAX_PROCS == 1) */
        movw    cc,d5             /* save condition codes */ 
        movl    CSYM(saved_pc),sp@-
        movw    d5,sp@-           /* push the saved condition codes */
        clrl    d5                /* generate a trap on the next limit check */
        rtr                       /* return, restoring condition codes */
#endif

/* array : (int * 'a) -> 'a array
 * Allocate and initialize a new array.	 This can cause GC.
 */
ML_CODE_HDR(array_a)
1:					/* jump back here after GC trap */
	movl	a0@,d1			/* get tagged length */
	asrl	#1,d1			/* d1 = length (untagged) */
	movl	d1,d2
	asll	#width_tags,d2
	orl	#TAG_array,d2		/* d2 = new tag */
	asll	#2,d1
	movl	a6,d4
	addl	d1,d4
        cmpl	d4,d5			/* check the heap limit */
        jpl 	4f
        lea	1b,a5
	movl	#closmask,d4
	rts
4:
	movl	a0@(4),d0		/* d0 = initial value */
	movl	d2,a6@+			/* write the tag */
	movl	a6,a0
	jra	3f
2:	movl	d0,a6@+			/* store default */
3:	subql	#4,d1
	jge 	2b
	CONTINUE

/* create_b : int -> bytearray
 * create_r : int -> realarray
 * create_s : int -> string
 * Create bytearray or string of given length.	This can cause GC.
 */
ML_CODE_HDR(create_r_a)
1:	movl	a0,d1
	asrl	#1,d1		/* d1 = # of elements */
	movl	d1,d2
	asll	#width_tags,d2
	asll	#3,d1		/* d1 = byte length, not including desc. */
	addl	#TAG_realdarray,d2		/* d2 = new tag */
	movl	a6,d4
	addl	d1,d4
	cmpl	d4,d5		/* check the heap limit */
        jpl 	4f
        lea	1b,a5
	movl	#closmask,d4
	rts
4:
	movl	d2,a6@+		/* write the descriptor */
	movl	a6,a0
	addl	d1,a6
	CONTINUE

ML_CODE_HDR(create_b_a)
1:				/* jump back to here after a GC trap */
	movl	a0,d1
	asrl	#1,d1		/* d1 = # of elements */
	movl	d1,d2
	asll	#width_tags,d2
	addl	#3,d1
	addl	#TAG_bytearray,d2		/* d2 = new tag */
	andl	#~3,d1		/* d1+4 = bytes in string, not including tag */
	movl	a6,d4
	addl	d1,d4
	cmpl	d4,d5		/* check the heap limit */
        jpl 	4f
        lea	1b,a5
	movl	#closmask,d4
	rts
4:
	movl	d2,a6@+		/* write the descriptor */
	movl	a6,a0
	addl	d1,a6
	CONTINUE

ML_CODE_HDR(create_s_a)
1:				/* jump back to here after a GC trap */
	movl	a0,d1
	asrl	#1,d1		/* d1 = # of elements */
	movl	d1,d2
	asll	#width_tags,d2
	addl	#4,d1
	addl	#TAG_string,d2		/* d2 = new tag */
	andl	#~3,d1		/* d1+4 = bytes in string, not including tag */
	movl	a6,d4
	addl	d1,d4
	cmpl	d4,d5		/* check the heap limit */
        jpl 	4f
        lea	1b,a5
	movl	#closmask,d4
	rts
4:
	movl	d2,a6@+		/* write the descriptor */
	movl	a6,a0
        movl    #0,a6@(-4)      /* clear the last word to zero */
	addl	d1,a6
	CONTINUE

/* create_v_a : int * 'a list -> 'a vector
 * 	creates a vector with elements taken from a list.
 *	n.b. The front end ensure that list cannot be nil.
 */
ML_CODE_HDR(create_v_a)
#define ML_NIL	1
#define ML_LIST_HD(p) 	p@
#define ML_LIST_TL(p) 	p@(4)
1:						/* jump back here after GC */
	movl 	stdarg@,d1			/* d1 := tagged length */
	asrl	#1,d1				/* untagged length */
	movl	d1,d2				/* build descriptor in d2 */
	asll	#width_tags,d2			/* length field */
	orl	#TAG_record,d2 			/* tag field */
	asll	#2,d1				/* d1 := length in bytes */
	movl	allocptr,d4	
	addl	d1,d4				/* d4 := new allocptr */
	cmpl	d4,limit			/* check heap limit */
        jpl 	4f
        lea	1b,a5
	movl	#closmask,d4
	rts
4:
	movl	d2,allocptr@+			/* store the descriptor */
	movl	stdarg@(4),ptrtmp		/* ptrtmp := list */
	movl	allocptr,stdarg 		/* return value */
	movl	#ML_NIL,d1			/* d1 := NIL */
3:						/* loop */
	movl	ML_LIST_HD(ptrtmp),allocptr@+ 	/* update vector */
	movl 	ML_LIST_TL(ptrtmp),ptrtmp	/* cdr list */
	cmpl	ptrtmp,d1			/* continue ? */
	bne	3b
	CONTINUE

/* try_lock: spin_lock -> bool. 
 * low-level test-and-set style primitive for mutual-exclusion among 
 * processors.  For now, we only provide a uni-processor trivial version.
 */
ML_CODE_HDR(try_lock_a)
#if (MAX_PROCS > 1)
	???
#else /* (MAX_PROCS == 1) */
	movl	a0@,d0			/* get old value of lock */
	movl	#1,a0@			/* set the lock to ML_false */
	movl	d0,a0			/* return old value of lock */
	CONTINUE
#endif

/* unlock : releases a spin lock 
 */
ML_CODE_HDR(unlock_a)
#if (MAX_PROCS > 1)
	???
#else /* (MAX_PROCS == 1) */
	movl	#3,a0@			/* store ML_true into lock */
	movl	#1,a0			/* and return unit         */
	CONTINUE
#endif


/* Floating point primitives
 *
 * All the m68code for ML reals assumes that NaN's and INF's are never
 * generated and therefore do not need to be handled.
 * This code does not produce NaN's or INF's as long as none are passed to it
 * and overflow, underflow, and operr are handled.
 *
 * Floating exceptions raised (assuming NaN's and INF's are never passed to
 * functions):
 *	OPERR - (div) for 0.0/0.0 (does NOT also cause DZ)
 *	DZ - (div) for (in range) / 0.0
 *	OVERFLOW/UNDERFLOW - (add,div,sub,mul) as appropriate */

ML_CODE_HDR(floor_a)
	fmoved	a0@,fp0
	ftstx	fp0		/* handle positive and negative cases separately*/
	fblt	1f
/* positive numbers */
	fintrzx	fp0,fp0		/* round towards zero (down) */
	fmovel	fp0,d0
	asll	#1,d0
	trapv
	addql	#1,d0
	movl	d0,a0
	CONTINUE
/* negative numbers */
1:	fintrzx	fp0,fp1		/* round towards zero (up) */
	fmovel	fp1,d0
        asll	#1,d0
	trapv
	fcmpx	fp0,fp1
	fjeq	1f
	subql	#1,d0
	trapv
	movl	d0,a0
	CONTINUE
1:	addql	#1,d0
	movl	d0,a0
	CONTINUE

/* returns 0 on 0. */
ML_CODE_HDR(logb_a)
	fgetexpd a0@,fp0
	fmovel	fp0,d0
	asll	#1,d0
	addql	#1,d0
	movl	d0,a0
	CONTINUE

ML_CODE_HDR(scalb_a)
	lea	CSYM(overflow_e0)+4,a0
	RAISE

#ifdef sun3
	.globl	CSYM(minitfp_)		/* checks for 68881 and sets flags */
	.globl	CSYM(fp_state_mc68881)	/* a flag that gets set */
#define fp_enabled 2 /* from /usr/src/lib/libc/sun/crt/fpcrttypes.h */
#endif
	.align	2
/* Enable/disable float operand error, overflow, and div.  If no 68881
   is present, nothing happens. */
	.globl CSYM(FPEnable)
	.globl CSYM(FPDisable)
CSYM(FPEnable):
#ifdef sun3
	jsr	CSYM(minitfp_)		/* checks for 68881 and sets flags.
					   normally executed on startup,
					   but won't be if compiled without
					   -f68881 (for possible sun/50
					   compatibility).  This is just
					   to make sure. */
	cmpl	#fp_enabled,CSYM(fp_state_mc68881)
	jne	1f
#endif
/* Sorry, but A/UX doesn't seem to handle fpu exceptions correctly. */
#ifndef AUX
	fmovel	#0x3400,fpcr
#endif
1:	rts
CSYM(FPDisable):
#ifdef sun3
	cmpl	#fp_enabled,CSYM(fp_state_mc68881)
	jne	1f
#endif sun3
#ifndef AUX
	fmovel	#0,fpcr
#endif
1:	rts

/* Mathematical routines */

ML_CODE_HDR(arctan_a)
	CHECKLIMIT(#closmask)
	fatand	a0@,fp0
        jra	finishfloat
ML_CODE_HDR(cos_a)
	CHECKLIMIT(#closmask)
	fcosd	a0@,fp0
        jra	finishfloat
ML_CODE_HDR(exp_a)
	CHECKLIMIT(#closmask)
	fetoxd	a0@,fp0
        jra	finishfloat
ML_CODE_HDR(ln_a)
	CHECKLIMIT(#closmask)
	fmoved  a0@,fp0
	ftstx	fp0
	fble	1f
	flognx	fp0,fp0
        jra	finishfloat
1:	lea	CSYM(ln_e0)+4,a0
	RAISE		
ML_CODE_HDR(sin_a)
	CHECKLIMIT(#closmask)
	fsind	a0@,fp0
        jra	finishfloat
ML_CODE_HDR(sqrt_a)
	CHECKLIMIT(#closmask)
	fmoved  a0@,fp0
	ftstx	fp0
	fblt	1f
	fsqrtx	fp0,fp0
        jra	finishfloat
1:	lea	CSYM(sqrt_e0)+4,a0
	RAISE		

finishfloat:			/* allocate a heap object for the result */
	movl	#DESC_reald,a6@+
	movl	a6,a0
	fmoved	fp0,a6@+
	CONTINUE


#ifdef NeXT
/* this is needed to avoid a bug in the NeXT's implementation of syscall */
	.globl	CSYM(syscall)
CSYM(syscall):
	movl	sp@(4),d0	/* get the syscall code */
	movl	sp@,sp@(4)
	movl	d0,sp@		/* the stack now is: syscall#, return-pc, args ... */
	trap	#0		/* do the syscall */
				/* the stack now is: return-pc, args ... */
	bcs	2f		/* check for errors */
1:				/* return */
	movl	sp@,a0
	jmp	a0@
2:				/* an error, save the errno and return -1 */
	movl	d0,CSYM(errno)
	moveq	#-1,d0
	jra	1b

#endif

/* this bogosity is for export.c */
	.globl	CSYM(startptr)
CSYM(startptr):
#ifdef AUX
	.long	_start
#else
	.long	start
#endif
#ifdef HPUX
 	/* pseudo-op to set floating
	   point version no in a.out
           header. Guessing...
	*/	
	version 3
#endif
