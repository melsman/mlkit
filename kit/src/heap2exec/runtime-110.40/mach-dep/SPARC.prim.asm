/* SPARC.prim.asm
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 *
 * AUTHOR:  John Reppy
 *	    Cornell University
 *	    Ithaca, NY 14853
 *	    jhr@cs.cornell.edu
 */

#include "asm-base.h"
#include "ml-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-request.h"
#include "ml-limits.h"
#include "mlstate-offsets.h"	/** this file is generated **/

/* SPARC runtime code for ML.  Registers are used as follows:
 *
 *	%g0	zero
 * 	%g1	standard link
 *	%g2-3  	misc regs
 *	%g4  	heap limit pointer
 * 	%g5  	store pointer
 *	%g6  	limit pointer
 *   	%g7  	exception handler
 *
 *	%o0-1	misc regs
 *	%o2	asm tmp
 *	%o3-5	misc regs
 *	%o6  	sp
 *   	%o7  	gcLink
 *
 *	%l0-7   misc regs	
 *
 *	%i0	standard arg
 * 	%i1	standard cont
 *	%i2  	standard clos
 *	%i3	base ptr
 *	%i4  	misc reg
 * 	%i5  	var ptr
 *	%i6  	fp (don't touch)
 *   	%i7  	misc reg 
 * 
 */


#define      ZERO %g0
#define   EXNCONT %g7		/* exception handler (ml_exncont) */
#define  ALLOCPTR %g6		/* freespace pointer (ml_allocptr) */
#define  STOREPTR %g5		/* store pointer     (ml_storeptr) */
#define  LIMITPTR %g4		/* heap limit pointer (ml_limitptr)*/
#define    STDARG %i0		/* standard argument (ml_arg)  	*/
#define   STDCONT %i1		/* standard continuation (ml_cont) */
#define   STDCLOS %i2		/* standard closure  (ml_clos)	*/
#define    VARPTR %i5		/* var pointer       (ml_varptr)   */
#define   STDLINK %g1		/* standard link     (ml_linkptr) */  
#define  MISCREG0 %g2
#define  MISCREG1 %g3
#define  MISCREG2 %o0
#define        PC %o7		/* ml_pc */

#define   ASMTMP  %o2		/* assembly temporary used in ML */
#define   TMPREG1 ASMTMP
#define   TMPREG2 %o3   
#define   TMPREG3 %o4
#define   TMPREG4 %o5

/* %o2 and %o3 are also used as for multiply and divide */

/*
 * %o6 = %sp (not used by ML)
 * %i6 = %fp (not used by ML)
 * %i7 = return address to C code (not used by ML)
 *
 * The ML stack frame has the following layout (set up by restoreregs):
 *
 *	%fp = %sp+4096
 *                      +-------------------+
 *                      |                   |	
 *                      .                   .
 *			|                   |
 *	%sp+116:	|  spill area       |
 *			+-------------------+
 *	%sp+112:	|     unused	    |
 *	%sp+108:	|     unused	    |
 *			+-------------------+
 *	%sp+104:	|     saved %o7     |
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
#define ML_FRAMESIZE 4096

#define      MUL_OFFSET 72
#define      DIV_OFFSET 76
#define     UMUL_OFFSET 80
#define     UDIV_OFFSET 84
#define    FLOOR_OFFSET 92
#define  MLSTATE_OFFSET 96
#define  STARTGC_OFFSET 100
#define      i7_OFFSET 104

#define CONTINUE				\
            jmp     STDCONT;			\
            subcc   ALLOCPTR,LIMITPTR,%g0

#define CHECKLIMIT(label)			\
		blu	label;			\
		nop;				\
 		mov	STDLINK,PC;		\
		ba	CSYM(saveregs);		\
		nop;				\
	label:


	TEXT

/* sigh_return_a:
 * The return continuation for the ML signal handler.
 */
ML_CODE_HDR(sigh_return_a)
	set	ML_unit,STDLINK
	set	ML_unit,STDCLOS
	set	ML_unit,PC
	ba	set_request
	set	REQ_SIG_RETURN,TMPREG3	/* (delay slot) */

/* sigh_resume:
 * Resume execution at the point at which a handler trap occurred.  This is a
 * standard two-argument function, thus the closure is in ml_cont (stdcont).
 */
ENTRY(sigh_resume)
	mov	STDLINK,PC
	ba	set_request
	set	REQ_SIG_RESUME,TMPREG3	/* (delay slot) */

/* pollh_return_a:
 * The return continuation for the ML poll handler.
 */
ML_CODE_HDR(pollh_return_a)
	set	ML_unit,STDLINK
	set	ML_unit,STDCLOS
	set	ML_unit,PC
	ba	set_request
	set	REQ_POLL_RETURN,TMPREG3	/* (delay slot) */

/* pollh_resume:
 * Resume execution at the point at which a poll event occurred.
 */
ENTRY(pollh_resume)
	mov	STDLINK,PC
	ba	set_request
	set	REQ_POLL_RESUME,TMPREG3	/* (delay slot) */

ML_CODE_HDR(handle_a)
	mov	STDLINK,PC
	ba	set_request
	set	REQ_EXN,TMPREG3		/* (delay slot) */

ML_CODE_HDR(return_a)
	set	ML_unit,STDLINK
	set	ML_unit,STDCLOS
	set	ML_unit,PC
	ba	set_request
	set	REQ_RETURN,TMPREG3		/* (delay slot) */

ENTRY(request_fault)
	mov	STDLINK,PC
	ba	set_request
	set	REQ_FAULT,TMPREG3		/* (delay slot) */

/* bind_cfun : (string * string) -> c_function
 */
ML_CODE_HDR(bind_cfun_a)
	CHECKLIMIT(bind_cfun_v_limit)
	ba	set_request
	set	REQ_BIND_CFUN,TMPREG3		/* (delay slot) */

ML_CODE_HDR(build_literals_a)
	CHECKLIMIT(build_literals_a_limit)
	ba	set_request
	set	REQ_BUILD_LITERALS,TMPREG3	/* (delay slot) */

ML_CODE_HDR(callc_a)
	CHECKLIMIT(callc_a_limit)
	ba	set_request
	set	REQ_CALLC,TMPREG3		/* (delay slot) */

/*
 * This is the entry point for starting gc called from ML's code.
 * I've added an adjustment to the return address.  The generated ML code
 * uses the JMPL instruction, which does not add an offset of 8 to the
 * correct return address.
 *
 *						Allen 6/5/1998
 */
ENTRY(saveregs0)
	add	PC, 8, PC
ENTRY(saveregs)
	set	REQ_GC,TMPREG3
	/* fall through */

set_request:
	ld	[%sp+MLSTATE_OFFSET],TMPREG2	/* get MLState ptr from stack */
	ld	[TMPREG2+VProcOffMSP],TMPREG1	/* TMPREG1 := VProcPtr */
	st	%g0,[TMPREG1+InMLOffVSP]	/* note that we have left ML code */
	st	ALLOCPTR,[TMPREG2+AllocPtrOffMSP]
	st	LIMITPTR,[TMPREG2+LimitPtrOffMSP]
	st	STOREPTR,[TMPREG2+StorePtrOffMSP]/* save storeptr */
	st	STDLINK,[TMPREG2+LinkRegOffMSP]
	st	PC,[TMPREG2+PCOffMSP]		/* PC of called function */
	st	STDARG,[TMPREG2+StdArgOffMSP]	/* save stdarg */
	st	STDCLOS,[TMPREG2+StdClosOffMSP]	/* save closure */
	st	STDCONT,[TMPREG2+StdContOffMSP]	/* save stdcont */
	st	VARPTR,[TMPREG2+VarPtrOffMSP]	/* save varptr */
	st	EXNCONT,[TMPREG2+ExnPtrOffMSP]	/* save exncont */
        st      MISCREG0,[TMPREG2+Misc0OffMSP]
        st      MISCREG1,[TMPREG2+Misc1OffMSP]
        st      MISCREG2,[TMPREG2+Misc2OffMSP]

        ldd	[%sp+64],%g6	    /* restore C registers %g6 & %g7.*/
	ld	[%sp+i7_OFFSET],%i7   /* restore C return address */
	mov	TMPREG3,%i0	    /* return request code */
	ret
	restore			    /* delay slot */


#define MLState ASMTMP
#define VProcPtr TMPREG4
ENTRY(restoreregs)
	save	%sp,-SA(ML_FRAMESIZE),%sp
	st	%i0,[%sp+MLSTATE_OFFSET]	/* save MLState ptr on stack */
	set	CSYM(saveregs0),ASMTMP
	st	ASMTMP,[%sp+STARTGC_OFFSET]
	mov	%i0,MLState			/* transfer MLState ptr to tmpreg4 */
	std	%g6,[%sp+64]			/* save C registers %g6 & %g7 */
	st	%i7, [%sp+i7_OFFSET]		/* save C return address */
	set	_ml_mul,TMPREG4			/* set pointer to ml_mul */
	st	TMPREG4,[%sp+MUL_OFFSET]
	set	_ml_div,TMPREG4			/* set pointer to ml_div */
	st	TMPREG4,[%sp+DIV_OFFSET]
	set	_ml_umul,TMPREG4		/* set pointer to ml_umul */
	st	TMPREG4,[%sp+UMUL_OFFSET]
	set	_ml_udiv,TMPREG4		/* set pointer to ml_udiv */
	st	TMPREG4,[%sp+UDIV_OFFSET]
	ld	[MLState+AllocPtrOffMSP],ALLOCPTR
	ld	[MLState+LimitPtrOffMSP],LIMITPTR
	ld	[MLState+StorePtrOffMSP],STOREPTR
	ld	[MLState+PCOffMSP],PC
	ld	[MLState+StdArgOffMSP],STDARG  
	ld	[MLState+StdContOffMSP],STDCONT
	ld	[MLState+StdClosOffMSP],STDCLOS     
	ld 	[MLState+VarPtrOffMSP],VARPTR
	ld	[MLState+LinkRegOffMSP],STDLINK
	ld	[MLState+ExnPtrOffMSP],EXNCONT	/* restore exnptr */
	ld	[MLState+Misc0OffMSP],MISCREG0
	ld	[MLState+Misc1OffMSP],MISCREG1
	ld	[MLState+Misc2OffMSP],MISCREG2
	ld	[MLState+VProcOffMSP],VProcPtr	/* TMPREG4 := VProcPtr */
	set	1,TMPREG2			/* note that we have entered ML code */
	st	TMPREG2,[VProcPtr+InMLOffVSP]
	ld	[VProcPtr+NPendingSysOffVSP],TMPREG2	/* check for pending signals */
	ld	[VProcPtr+NPendingOffVSP],TMPREG3
	addcc	TMPREG2,TMPREG3,%g0
	bne	pending_sigs
	nop
CSYM(ml_go):					/* invoke the ML code */
	jmp	PC
	subcc	ALLOCPTR,LIMITPTR,%g0	    /* Heap limit test (delay slot) */

pending_sigs:	/* there are pending signals */
					/* check if we are currently handling a signal */
	ld	[VProcPtr+InSigHandlerOffVSP],TMPREG2
	tst	TMPREG2
	bne	ml_go
	set	1,TMPREG2			    /* (delay slot) */
					/* note that a handler trap is pending */
	st	TMPREG2,[VProcPtr+HandlerPendingOffVSP]
	ba	ml_go
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
	CHECKLIMIT(array_a_limit)
	ld	[STDARG],TMPREG1		/* tmp1 = length in words */
	sra	TMPREG1,1,TMPREG2		/* tmp2 = length (untagged) */
	cmp	TMPREG2,SMALL_OBJ_SZW	    	/* is this a small object? */
	bgt	3f
	nop
    /** allocate and initialize array data **/
	ld	[STDARG+4],STDARG		/* stdarg = initial value */
	sll	TMPREG2,TAG_SHIFTW,TMPREG3	/* build descriptor in tmp3 */
	or	TMPREG3,MAKE_TAG(DTAG_arr_data),TMPREG3
	st	TMPREG3,[ALLOCPTR]		/* store the descriptor */
	inc	4,ALLOCPTR			/* allocptr++ */
	mov	ALLOCPTR,TMPREG3		/* array data ptr in tmp3 */
1:						/* loop */
	st	STDARG,[ALLOCPTR]
	deccc	1,TMPREG2			  /* if (--length > 0) */
	bgt	1b				    /* then continue */
	inc	4,ALLOCPTR			    /* allocptr++ (delay slot) */
						/* end loop */
    /** allocate array header **/
	set	DESC_polyarr,TMPREG2		/* descriptor in tmp2 */
	st	TMPREG2,[ALLOCPTR]		/* store the descriptor */
	inc	4,ALLOCPTR			/* allocptr++ */
	mov	ALLOCPTR,STDARG			/* result = header addr */
	st	TMPREG3,[ALLOCPTR]		/* store pointer to data */
	st	TMPREG1,[ALLOCPTR+4]
	inc	8,ALLOCPTR			/* allocptr += 2 */
	CONTINUE

3:	/* here we do off-line allocation for big arrays */
 	mov	STDLINK,PC
	ba	set_request
	set	REQ_ALLOC_ARRAY,TMPREG3	    /* (delayslot) */

/* create_r : int -> realarray
 * Create a new realarray.
 */
ML_CODE_HDR(create_r_a)
	CHECKLIMIT(create_r_a_limit)
	sra	STDARG,1,TMPREG2		/* tmp2 = length (untagged int) */
	sll	TMPREG2,1,TMPREG2		/* tmp2 = length in words */
	cmp	TMPREG2,SMALL_OBJ_SZW	        /* is this a small object? */
	bgt	1f
	nop
      /* Allocate the data object */
	sll	TMPREG2,TAG_SHIFTW,TMPREG1	/* build data desc in tmp1 */
	or	TMPREG1,MAKE_TAG(DTAG_raw64),TMPREG1
#ifdef ALIGN_REALDS
	or	ALLOCPTR,0x4,ALLOCPTR		/* desc is unaliged */
#endif
	st	TMPREG1,[ALLOCPTR]		/* store the data descriptor */
	inc	4,ALLOCPTR			/* allocptr++ */
	mov	ALLOCPTR,TMPREG3		/* tmp3 = data object */
	sll	TMPREG2,2,TMPREG2		/* tmp2 = length in bytes */
	add	ALLOCPTR,TMPREG2,ALLOCPTR	/* allocptr += length */
      /* Allocate the header object */
	set	DESC_real64arr,TMPREG1
	st	TMPREG1,[ALLOCPTR]		/* header descriptor */
	inc	4,ALLOCPTR			/* allocptr++ */
	st	TMPREG3,[ALLOCPTR]		/* header data field */
	st	STDARG,[ALLOCPTR+4]		/* header length field */
	mov	ALLOCPTR,STDARG			/* stdarg = header object */
	inc	8,ALLOCPTR			/* allocptr += 2 */
	CONTINUE

1:	/* off-line allocation of big realarrays */
 	mov	STDLINK,PC
	ba	set_request
	set	REQ_ALLOC_REALDARRAY,TMPREG3	/* (delayslot) */

/* create_b : int -> bytearray
 * Create a bytearray of the given length.
 */
ML_CODE_HDR(create_b_a)
	CHECKLIMIT(create_b_a_limit)
	sra	STDARG,1,TMPREG2		/* tmp2 = length (sparc int) */
	add	TMPREG2,3,TMPREG2		/* tmp2 = length in words */
	sra	TMPREG2,2,TMPREG2
	cmp	TMPREG2,SMALL_OBJ_SZW		/* is this a small object? */
	bgt	1f
	nop
      /* Allocate the data object */
	sll	TMPREG2,TAG_SHIFTW,TMPREG1	/* build data desc in tmp1 */
	or	TMPREG1,MAKE_TAG(DTAG_raw32),TMPREG1
	st	TMPREG1,[ALLOCPTR]		/* store the data descriptor */
	inc	4,ALLOCPTR			/* allocptr++ */
	mov	ALLOCPTR,TMPREG3		/* tmp3 = data object */
	sll	TMPREG2,2,TMPREG2		/* tmp2 = length in bytes */
	add	ALLOCPTR,TMPREG2,ALLOCPTR	/* allocptr += length */
      /* Allocate the header object */
	set	DESC_word8arr,TMPREG1		/* header descriptor */
	st	TMPREG1,[ALLOCPTR]
	inc	4,ALLOCPTR			/* allocptr++ */
	st	TMPREG3,[ALLOCPTR]		/* header data field */
	st	STDARG,[ALLOCPTR+4]		/* header length field */
	mov	ALLOCPTR,STDARG			/* stdarg = header object */
	inc	8,ALLOCPTR			/* allocptr += 2 */
	CONTINUE

1:	/* here we do off-line allocation for big bytearrays */
 	mov	STDLINK,PC
	ba	set_request
	set	REQ_ALLOC_BYTEARRAY,TMPREG3	/* (delayslot) */

/* create_s : int -> string
 * Create a string of the given length (> 0).
 */
ML_CODE_HDR(create_s_a)
	CHECKLIMIT(create_s_a_limit)
	sra	STDARG,1,TMPREG2		/* tmp2 = length (sparc int) */
	add	TMPREG2,4,TMPREG2		/* tmp2 = length in words */
						/*   (including zero at end). */
	sra	TMPREG2,2,TMPREG2
	cmp	TMPREG2,SMALL_OBJ_SZW		/* is this a small object? */
	bgt	1f
	nop
      /* Allocate the data object */
	sll	TMPREG2,TAG_SHIFTW,TMPREG1	/* build data desc in tmp1 */
	or	TMPREG1,MAKE_TAG(DTAG_raw32),TMPREG1
	st	TMPREG1,[ALLOCPTR]		/* store the data descriptor */
	inc	4,ALLOCPTR			/* allocptr++ */
	mov	ALLOCPTR,TMPREG3		/* tmp3 = data object */
	sll	TMPREG2,2,TMPREG2		/* tmp2 = length in bytes */
	add	ALLOCPTR,TMPREG2,ALLOCPTR	/* allocptr += length */
	st	%g0,[ALLOCPTR-4]		/* store 0 in last word of data */
      /* Allocate the header object */
	set	DESC_string,TMPREG1		/* header descriptor */
	st	TMPREG1,[ALLOCPTR]
	inc	4,ALLOCPTR			/* allocptr++ */
	st	TMPREG3,[ALLOCPTR]		/* header data field */
	st	STDARG,[ALLOCPTR+4]		/* header length field */
	mov	ALLOCPTR,STDARG			/* stdarg = header object */
	inc	8,ALLOCPTR			/* allocptr += 2 */
	CONTINUE

1:	/* here we do off-line allocation for big strings */
 	mov	STDLINK,PC
	ba	set_request
	set	REQ_ALLOC_STRING,TMPREG3	/* (delayslot) */

/* create_v_a : (int * 'a list) -> 'a vector
 * Create a vector with elements taken from a list.
 * NOTE: the front-end ensures that list cannot be nil.
 */
ML_CODE_HDR(create_v_a)
	CHECKLIMIT(create_v_a_limit)
	ld	[STDARG],TMPREG1		/* tmp1 = length (tagged int) */
	sra	TMPREG1,1,TMPREG2		/* tmp2 = length (untagged int) */
	cmp	TMPREG2,SMALL_OBJ_SZW		/* is this a small object? */
	bgt	1f
	nop
      /* allocate and initialize data object */
	sll	TMPREG2,TAG_SHIFTW,TMPREG2	/* build descriptor in TMPREG2 */
	or	TMPREG2,MAKE_TAG(DTAG_vec_data),TMPREG2
	st	TMPREG2,[ALLOCPTR]		/* store the descriptor */
	inc	4,ALLOCPTR			/* allocptr++ */
	ld	[STDARG+4],TMPREG2		/* tmp2 = list */
	mov	ALLOCPTR,STDARG			/* stdarg = data obj */
2:						/* loop */
	ld	[TMPREG2],TMPREG3		  /* tmp3 = hd(tmp2) */
	ld	[TMPREG2+4],TMPREG2		  /* tmp2 = tl(tmp2) */
	st	TMPREG3,[ALLOCPTR]		  /* store element */
	cmp	TMPREG2,ML_nil			  /* if (tmp2 <> nil) goto loop */
	bne	2b
	inc	4,ALLOCPTR			  /* allocptr++ (delay slot) */
						/* end loop */
      /* allocate header object */
	set	DESC_polyvec,TMPREG3		/* descriptor in TMPREG3 */
	st	TMPREG3,[ALLOCPTR]		/* header descriptor */
	inc	4,ALLOCPTR			/* allocptr++ */
	st	STDARG,[ALLOCPTR]		/* header data field */
	st	TMPREG1,[ALLOCPTR+4]		/* header length field */
	mov	ALLOCPTR,STDARG			/* result = header object */
	inc	8,ALLOCPTR			/* allocptr += 2 */
	CONTINUE

1:	/* off-line allocation of big vectors */
 	mov	STDLINK,PC
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
	CHECKLIMIT(scalb_a_limit)
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
	set	ML_false,TMPREG4	/* ML_false */
	st	TMPREG4,[STDARG]	/* store ML_false into the lock */
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
