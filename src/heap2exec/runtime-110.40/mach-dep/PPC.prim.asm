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
#include "ml-limits.h"
#include "mlstate-offsets.h"	/** this file is generated **/

/* factor out variations in assembler synatax */
#if defined(OPSYS_DARWIN)
   /* use the macOS X names by default */
#  define HI(name) ha16(name)
#  define LO(name) lo16(name)


#else 
#  define HI(name) name@ha
#  define LO(name) name@l

#  define cr0 0
#  define cr1 1
#  define r0  0
#  define r1  1
#  define r2  2
#  define r3  3
#  define r4  4
#  define r5  5
#  define r6  6
#  define r7  7
#  define r8  8
#  define r9  9
#  define r10  10
#  define r11  11
#  define r12  12
#  define r13  13
#  define r14  14
#  define r15  15
#  define r16  16
#  define r17  17
#  define r18  18
#  define r19  19
#  define r20  20
#  define r21  21
#  define r22  22
#  define r23  23
#  define r24  24
#  define r25  25
#  define r26  26
#  define r27  27
#  define r28  28
#  define r29  29
#  define r30  30
#  define r31  31

#  define f0  0
#  define f1  1
#  define f2  2
#  define f3  3
#  define f4  4
#  define f5  5
#  define f6  6
#  define f7  7
#  define f8  8
#  define f9  9
#  define f10  10
#  define f11  11
#  define f12  12
#  define f13  13
#  define f14  14
#  define f15  15
#  define f16  16
#  define f17  17
#  define f18  18
#  define f19  19
#  define f20  20
#  define f21  21
#  define f22  22
#  define f23  23
#  define f24  24
#  define f25  25
#  define f26  26
#  define f27  27
#  define f28  28
#  define f29  29
#  define f30  30
#  define f31  31

#endif


	
/** register usage **/
#define		sp		r1
#define 	stackptr	sp

#define		allocptr	r14
#define 	limitptr	r15
#define 	storeptr	r16
#define		stdlink		r17
#define 	stdclos		r18
#define 	stdarg		r19
#define 	stdcont		r20
#define 	exncont		r21
#define 	varptr		r22
#define		miscreg0	r24
#define		miscreg1	r25
#define 	miscreg2	r26

#define		pc		r28


#define       	atmp1 r29
#define       	atmp2 r30
#define       	atmp3 r31
#define 	atmp4 r13

/* stackframe layout:
 * Note: 1. The offset of cvti2d tmp is used in rs6000.sml
 *          float load/store offset is hardwired in rs6000instr.sml
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
 *	  40(sp)     | unused		 |
 *		     +-------------------+
 *	  44(sp)     | unused		 |
 *		     +-------------------+
 *  argblock(sp)     | C calleesave regs |
 *	             .		         .
 *		     .		         .
 *		     +-------------------+
 *  argblock+92(sp)  |		    	 |
 */


/** MLState offsets **/
#define argblock 		48
#define savearea		(23*4+4)	/* lr,cr,1,2,13-31,padding */
#define framesize		4096
#define MLSTATE_OFFSET 		0	
#define STARTGC_OFFSET		4
#define CVTI2D_OFFSET		8
#define FLOOR_OFFSET		32

/** offsets in condition register CR.0 **/

#define CR0_LT 0
#define CR0_GT 1
#define CR0_EQ 2
#define CR0_SO 3
	
#define CR0	cr0


/** C parameter passing conventions **/
#define CARG1 		r3
#define CRESULT1 	r3


#define CONTINUE					\
	    cmpl	CR0,allocptr,limitptr __SC__	\
	    mtlr	stdcont __SC__			\
	    blr

#define CHECKLIMIT(label)	 			\
	    bt		CR0_LT, label __SC__		\
	    addi	pc, stdlink,0 __SC__		\
	    b		CSYM(saveregs) __SC__	\
    label:

#if defined (USE_TOC)
/* create table of contents entries for things we need the address of. */
	.extern		CSYM(_PollFreq0)
	.extern 	CSYM(_PollEvent0)
	.extern		CSYM(saveregs)

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
	li 	atmp4,REQ_SIG_RETURN
	li	stdlink, ML_unit
	li	stdclos, ML_unit
	li	pc, ML_unit
	b	set_request

ENTRY(sigh_resume)
	li	atmp4, REQ_SIG_RESUME
	b	set_request

/* pollh_return_a:
 * The return continuation for the ML poll handler.
 */
ML_CODE_HDR(pollh_return_a)
	li	atmp4,REQ_POLL_RETURN
	li	stdlink, ML_unit
	li	stdclos, ML_unit
	li	pc, ML_unit
	b	set_request

/* pollh_resume:
 * Resume execution at the point at which a poll event occurred.
 */
ENTRY(pollh_resume)
	li	atmp4,REQ_POLL_RESUME
	b	set_request

		 /* exception handler for ML functions called from C */
ML_CODE_HDR(handle_a)
	li	atmp4,REQ_EXN
	addi	pc, stdlink, 0
	b	set_request


		/* continuation for ML functions called from C */		
ML_CODE_HDR(return_a)
	li	atmp4,REQ_RETURN
	li	stdlink, ML_unit
	li	stdclos, ML_unit
	li	pc, ML_unit
	b	set_request


ENTRY(request_fault)
	li	atmp4,REQ_FAULT
	addi	pc, stdlink, 0
	b	set_request


/* bind_cfun : (string * string) -> c_function
 */
ML_CODE_HDR(bind_cfun_a)
	CHECKLIMIT(bind_cfun_v_limit) 
	li	atmp4,REQ_BIND_CFUN
	b	set_request

ML_CODE_HDR(build_literals_a)
	CHECKLIMIT(build_literals_v_limit) 
	li	atmp4,REQ_BUILD_LITERALS
	b	set_request

ML_CODE_HDR(callc_a)
	CHECKLIMIT(callc_v_limit) 
	li	atmp4,REQ_CALLC
	b	set_request


ENTRY(saveregs)
	li	atmp4, REQ_GC
	mflr	pc
	/* fall through */

set_request:
	lwz	atmp3,MLSTATE_OFFSET(sp)	/* save the minimal ML state */
	lwz	atmp2,VProcOffMSP(atmp3)	/* atmp2 := VProc State ptr */
	li	r0,0
	stw	r0,InMLOffVSP(atmp2)		/* note that we have left ML */
	stw	allocptr,AllocPtrOffMSP(atmp3)
	stw	limitptr,LimitPtrOffMSP(atmp3)
	stw	storeptr,StorePtrOffMSP(atmp3)
	stw	stdlink,LinkRegOffMSP(atmp3)
	stw	pc,PCOffMSP(atmp3)
	stw	stdarg,StdArgOffMSP(atmp3)
	stw	stdcont,StdContOffMSP(atmp3)
	stw	stdclos,StdClosOffMSP(atmp3)
	stw	varptr,VarPtrOffMSP(atmp3)
	stw	exncont,ExnPtrOffMSP(atmp3)
	stw	miscreg0,Misc0OffMSP(atmp3)
	stw	miscreg1,Misc1OffMSP(atmp3)
	stw	miscreg2,Misc2OffMSP(atmp3)

	addi	r3,atmp4,0			/* request as argument */

restore_c_regs:
 	lwz	r2, (argblock+4)(sp) 
	lwz	r13, (argblock+8)(sp)
	lwz	r14, (argblock+12)(sp)
	lwz	r15, (argblock+16)(sp)
	lwz	r16, (argblock+20)(sp)
	lwz	r17, (argblock+24)(sp)
	lwz	r18, (argblock+28)(sp)
	lwz	r19, (argblock+32)(sp)
	lwz	r20, (argblock+36)(sp)
	lwz	r21, (argblock+40)(sp)
	lwz	r22, (argblock+44)(sp)
	lwz	r23, (argblock+48)(sp)
	lwz	r24, (argblock+52)(sp)
	lwz	r25, (argblock+56)(sp)
	lwz	r26, (argblock+60)(sp)
	lwz	r27, (argblock+64)(sp)
	lwz	r28, (argblock+68)(sp)
	lwz	r29, (argblock+72)(sp)
	lwz	r30, (argblock+76)(sp)
	lwz	r31, (argblock+80)(sp)
	lwz	r0, (argblock+84)(sp)
	mtlr    r0
	lwz	r0, (argblock+88)(sp)
	mtcrf	0x80, r0
	addi	sp,sp,framesize 
	blr



CENTRY(restoreregs)
	addi	sp,sp,-framesize
#if defined(USE_TOC)
	lwz	r0,T.saveregs(2)
#else
	lis	r28, HI(CSYM(saveregs))	/* GPR0 <- addrof(saveregs) */
	addi	r28, r28, LO(CSYM(saveregs))
        li      r0, 0
        add     r0, r28, r0
#endif
	stw	r3, MLSTATE_OFFSET(sp)
	stw	r0, STARTGC_OFFSET(sp)
#if defined(USE_TOC)
	lwz	r4, T.cvti2d_CONST(r2)		/* GPR2 is RTOC */
	lfd	f0, 0(r4)
#else
	lis	r4, HI(cvti2d_CONST)
	lfd	f0, LO(cvti2d_CONST)(r4)
#endif
	stfd	f0, CVTI2D_OFFSET(sp)

	stw	r2, argblock+4(sp)
	stw	r13, argblock+8(sp)
	stw	r14, argblock+12(sp)
	stw	r15, argblock+16(sp)
	stw	r16, argblock+20(sp)
	stw	r17, argblock+24(sp)
	stw	r18, argblock+28(sp)
	stw	r19, argblock+32(sp)
	stw	r20, argblock+36(sp)
	stw	r21, argblock+40(sp)
	stw	r22, argblock+44(sp)
	stw	r23, argblock+48(sp)
	stw	r24, argblock+52(sp)
	stw	r25, argblock+56(sp)
	stw	r26, argblock+60(sp)
	stw	r27, argblock+64(sp)
	stw	r28, argblock+68(sp)
	stw	r29, argblock+72(sp)
	stw	r30, argblock+76(sp)
	stw	r31, argblock+80(sp)
	mflr    r0
	stw	r0,  argblock+84(sp)
	mfcr	r0
	stw	r0,  argblock+88(sp)
	
	and	atmp1,r3,r3			/* atmp1 := MLState pointer */

	lwz	allocptr,AllocPtrOffMSP(atmp1)
	lwz	limitptr,LimitPtrOffMSP(atmp1)
	lwz	storeptr,StorePtrOffMSP(atmp1)
	lwz	atmp2,VProcOffMSP(atmp1)	/* atmp2 := VProc State ptr */
	li	atmp3,1
	stw	atmp3,InMLOffVSP(atmp2)         /* we are entering ML code */
	lwz	stdarg,StdArgOffMSP(atmp1)
	lwz	stdcont,StdContOffMSP(atmp1)
	lwz	stdclos,StdClosOffMSP(atmp1)
	lwz	exncont,ExnPtrOffMSP(atmp1)
	lwz	miscreg0,Misc0OffMSP(atmp1)
	lwz	miscreg1,Misc1OffMSP(atmp1)
	lwz	miscreg2,Misc2OffMSP(atmp1)
	lwz	stdlink,LinkRegOffMSP(atmp1)
	lwz	varptr,VarPtrOffMSP(atmp1)
	lwz	atmp3,PCOffMSP(atmp1)
	mtlr	atmp3
						/* check for pending signals */
	lwz	atmp1,NPendingSysOffVSP(atmp2)
	lwz	atmp3,NPendingOffVSP(atmp2)
	add	atmp1,atmp1,atmp3
	cmpi	CR0,atmp1,0
	bf	CR0_EQ,pending_sigs


ENTRY(ml_go) 
	cmpl	CR0,allocptr,limitptr
	mtfsfi  3,0			/* Ensure that no exceptions are set */
	mtfsfi  2,0
	mtfsfi  1,0
	mtfsfi  0,0
	li	r0,0
	mtxer   r0
	blr				/* jump to ML code */

pending_sigs:				/* there are pending signals */
	lwz	atmp1,InSigHandlerOffVSP(atmp2)
	cmpi	CR0,atmp1,0
	bf 	CR0_EQ,CSYM(ml_go)
					/* check if currently handling a signal */	
	lwz	atmp1,InSigHandlerOffVSP(atmp2)
	cmpi	CR0,atmp1,0
	bf	CR0_EQ,CSYM(ml_go)

	li	r0,1
	stw	r0,HandlerPendingOffVSP(atmp2)
	addi	limitptr,allocptr,0
	b	CSYM(ml_go)

/* array : (int * 'a) -> 'a array
 * Allocate and initialize a new array.	 This can cause GC.
 */
ML_CODE_HDR(array_a)
	CHECKLIMIT(array_a_limit)

	lwz	atmp1,0(stdarg)		/* atmp1 := length in words */
	srawi	atmp2, atmp1, 1		/* atmp2 := length (untagged) */
	cmpi	CR0,atmp2,SMALL_OBJ_SZW /* is this a small object */
	bf	CR0_LT, array_a_large

	lwz	stdarg,4(stdarg)	/* initial value */
	slwi	atmp3,atmp2,TAG_SHIFTW	/* build descriptor in tmp3 */
	ori	atmp3,atmp3,MAKE_TAG(DTAG_arr_data)
	stw	atmp3,0(allocptr)	/* store descriptor */
	addi	allocptr,allocptr,4	/* points to new object */
	addi	atmp3,allocptr,0	/* array data ptr in atmp3 */

array_a_1:
	stw	stdarg,0(allocptr)	/* initialize array */
	addi	atmp2,atmp2,-1
	addi	allocptr,allocptr,4
	cmpi 	CR0,atmp2,0
	bf	CR0_EQ,array_a_1

	/* allocate array header */
	li	atmp2,DESC_polyarr	/* descriptor in tmp2 */
	stw	atmp2,0(allocptr)	/* store descriptor */
	addi	allocptr, allocptr, 4	/* allocptr++ */
	addi	stdarg, allocptr, 0	/* result = header addr */
	stw	atmp3,0(allocptr)	/* store pointer to data */
	stw 	atmp1,4(allocptr)
	addi	allocptr,allocptr,8
	CONTINUE
array_a_large:				/* off-line allocation */
	li	atmp4,REQ_ALLOC_ARRAY
	addi	pc, stdlink,0
	b	set_request

/* create_b : int -> bytearray
 * Create a bytearray of the given length.
 */
ML_CODE_HDR(create_b_a)
	CHECKLIMIT(create_b_a_limit)

	srawi	atmp2,stdarg,1		/* atmp2 = length (untagged int) */
	addi	atmp2,atmp2,3		/* atmp2 = length in words */
	srawi	atmp2,atmp2,2		
	cmpi    CR0,atmp2,SMALL_OBJ_SZW /* is this a small object */
	bf     CR0_LT,create_b_a_large

	/* allocate the data object */
	slwi	atmp1,atmp2,TAG_SHIFTW	/* build descriptor in atmp1 */
	ori	atmp1,atmp1,MAKE_TAG(DTAG_raw32)
	stw	atmp1,0(allocptr) 	/* store the data descriptor */
	addi	allocptr,allocptr,4	/* allocptr++ */
	addi	atmp3, allocptr, 0	/* atmp3 = data object */
	slwi	atmp2, atmp2, 2		/* atmp2 = length in bytes */
	add	allocptr,allocptr,atmp2 /* allocptr += total length */

	/* allocate the header object */
	li	atmp1, DESC_word8arr	/* header descriptor */
	stw	atmp1,0(allocptr)	
	addi	allocptr, allocptr, 4	/* allocptr++ */
	stw	atmp3,0(allocptr)	/* header data field */
	stw	stdarg,4(allocptr)	/* header length field */
	addi	stdarg, allocptr, 0	/* stdarg = header object */
	addi	allocptr,allocptr,8	/* allocptr += 2 */
	CONTINUE

create_b_a_large:			/* off-line allocation */
	li 	atmp4,REQ_ALLOC_BYTEARRAY
	addi	pc, stdlink,0
	b	set_request


/*
** create_s_a: int -> string
*/
ML_CODE_HDR(create_s_a)
	CHECKLIMIT(create_s_a_limit)

	srawi	atmp2,stdarg,1		/* atmp2 = length(untagged int) */
	addi	atmp2,atmp2,4
	srawi	atmp2,atmp2,2		/* length in words (including desc) */
	cmpi	CR0,atmp2,SMALL_OBJ_SZW /* is this a small object */
	bf	CR0_LT,create_s_a_large
	
	slwi	atmp1,atmp2,TAG_SHIFTW	/* build descriptor in atmp3 */
	ori	atmp1,atmp1,MAKE_TAG(DTAG_raw32)
	stw	atmp1,0(allocptr)	/* store descriptor */
	addi	allocptr,allocptr,4	/* allocptr++ */
	addi	atmp3,allocptr,0	/* atmp3 = data object */
	slwi	atmp2,atmp2,2		/* atmp2 = length in bytes */
	add	allocptr,atmp2,allocptr /* allocptr += total length */
	stw	r0,-4(allocptr)		/* store zero in last word */

	/* Allocate the header object */
	li	atmp1, DESC_string	/* header descriptor */
	stw	atmp1, 0(allocptr)
	addi	allocptr,allocptr,4	/* allocptr++ */
	stw	atmp3,0(allocptr)	/* header data field */
	stw	stdarg,4(allocptr)	/* header length field */
	addi	stdarg,allocptr,0	/* stdarg = header object */
	addi	allocptr,allocptr,8	/* allocptr += 2 */
	CONTINUE

create_s_a_large:			/* off-line allocation */
	li	atmp4,REQ_ALLOC_STRING
	addi	pc, stdlink,0
	b	set_request



ML_CODE_HDR(create_r_a)
	CHECKLIMIT(create_r_a_limit)

	srawi	atmp2,stdarg,1		/* atmp2 = length (untagged int) */
	slwi	atmp2,atmp2,1		/* length in words */
	cmpi	CR0,atmp2,SMALL_OBJ_SZW	/* is this a small object */
	bf	CR0_LT,create_r_a_large
	
	/* allocate the data object */
	slwi	atmp1, atmp2, TAG_SHIFTW /* descriptor in atmp1 */
	ori	atmp1, atmp1, MAKE_TAG(DTAG_raw64)
#ifdef ALIGN_REALDS
	ori	allocptr,allocptr,4
#endif	
	stw	atmp1,0(allocptr)	/* store the descriptor */
	addi	allocptr, allocptr, 4	/* allocptr++ */
	addi	atmp3, allocptr, 0	/* atmp3 = data object */
	slwi	atmp2, atmp2, 2		/* tmp2 = length in bytes */
	add	allocptr,allocptr,atmp2 /* allocptr += length */

	/* allocate the header object */
	li	atmp1, DESC_real64arr
	stw	atmp1, 0(allocptr)	/* header descriptor */
	addi	allocptr,allocptr,4	/* allocptr++ */
	stw	atmp3,0(allocptr)	/* header data field */
	stw	stdarg,4(allocptr)	/* header length field */
	addi	stdarg,allocptr,0	/* stdarg = header object */
	addi	allocptr,allocptr,8	/* allocptr += 2 */
	CONTINUE
create_r_a_large:			/* offline allocation */
	li	atmp4,REQ_ALLOC_REALDARRAY
	addi	pc, stdlink,0
	b	set_request


/* create_v_a : (int * 'a list) -> 'a vector
 * Create a vector with elements taken from a list.
 * NOTE: the front-end ensures that list cannot be nil.
 */
ML_CODE_HDR(create_v_a)
	CHECKLIMIT(create_v_a_limit)
	
	lwz	atmp1,0(stdarg)		/* atmp1 = tagged length */
	srawi	atmp2,atmp1,1		/* atmp2 = untagged length */
	cmpi	CR0,atmp2,SMALL_OBJ_SZW /* is this a small object */
	bf	CR0_LT,create_v_a_large

	slwi	atmp2,atmp2,TAG_SHIFTW	/* build descriptor in atmp2 */
	ori	atmp2,atmp2,MAKE_TAG(DTAG_vec_data)
	stw	atmp2,0(allocptr)	/* store descriptor */
	addi	allocptr,allocptr,4	/* allocptr++ */
	lwz	atmp2,4(stdarg)		/* atmp2 := list */
	addi	stdarg,allocptr,0	/* stdarg := vector */

create_v_a_1:
	lwz	atmp3,0(atmp2)		/* atmp3:=hd(atmp2) */
	lwz	atmp2,4(atmp2)		/* atmp2:=tl(atmp2) */
	stw	atmp3,0(allocptr)	/* store word */
	addi	allocptr,allocptr,4	/* allocptr++ */
	cmpi	CR0,atmp2,ML_nil
	bf	CR0_EQ,create_v_a_1

	/* allocate header object */
	li	atmp3, DESC_polyvec	/* descriptor in tmp3 */
	stw	atmp3,0(allocptr)	/* store descriptor */
	addi	allocptr,allocptr,4	/* allocptr++ */
	stw	stdarg,0(allocptr)	/* header data field */
	stw 	atmp1,4(allocptr)	/* header length */
	addi	stdarg, allocptr, 0	/* result = header object */
	addi	allocptr,allocptr,8	/* allocptr += 2 */
	CONTINUE

create_v_a_large:
	li	atmp4,REQ_ALLOC_VECTOR
	addi	pc, stdlink,0
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
	lfd	f1, 0(stdarg)		
	/*
	** Neat thing here is that this code works for
	** both +ve and -ve floating point numbers.
	*/
	mffs	f0
	stfd	f0,0(allocptr)	/* steal the allocptr for a second */
	lwz	r0, 4(allocptr)
	mtfsb1	30
	mtfsb1 	31
#ifdef USE_TOC
	lwz	atmp1, T.floor_CONST(r2)
	lfd	f3, 0(atmp1)
#else
	lis	atmp1, HI(floor_CONST)
	lfd	f3, LO(floor_CONST)(atmp1)
#endif
	fadd	f6,f1,f3
	stfd	f6,FLOOR_OFFSET(sp)
	lwz	stdarg,FLOOR_OFFSET+4(sp)
	add	stdarg,stdarg,stdarg
	addi	stdarg,stdarg,1
	
	andi.	r0,r0, 0xf
	mtfsf	0xff,f0
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
	CHECKLIMIT(scalb_v_limit)
	lwz	atmp1,4(stdarg)		/* atmp1 := y */
	srawi	atmp1,atmp1,1		/* atmp1 := machine int y */
	lwz	stdarg,0(stdarg)	/* stdarg := x */
	lwz	atmp2,0(stdarg)		/* atmp2 := MSW(x) */
	lis	r0,0x7ff0		/* r0 := 0x7ff0,0000 */
	and.	atmp3,atmp2,r0		/* atmp3 := atmp2 & 0x7ff00000 */
	bt	CR0_EQ,scalb_all_done
	
	srawi	atmp3,atmp3,20		/* atmp3 := ieee(exp) */
	add.	atmp1,atmp1,atmp3	/* scale exponent */
	bt	CR0_LT,scalb_underflow

	cmpi	CR0,atmp1,2047		/* max. ieee(exp) */
	bf	CR0_LT,scalb_overflow

	not	r0,r0			/* r0 := not(r0) */
	and	atmp2,atmp2,r0		/* atmp2 := high mantessa bits + sign */
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



CENTRY(set_fsr)
	mtfsb0	24		/* disable invalid exception */
	mtfsb0	25		/* disable overflow exception */
	mtfsb0	26		/* disable underflow exception */
	mtfsb0	28		/* disable inexact exception */
	mtfsb0	30		/* round to nearest */
	mtfsb0	31		
	blr			/* return */
	
/* saveFPRegs and restoreFPRegs are called from C only. */
#define ctmp1 12
#define ctmp2 11
#define ctmp3 10


CENTRY(SaveFPRegs)
	stfd	f14, 4(r3)
	stfd	f15, 12(r3)
	stfd	f16, 20(r3)
	stfd	f17, 28(r3)
	stfd	f18, 36(r3)
	stfd	f19, 44(r3)
	stfd	f20, 52(r3)
	stfd	f21, 60(r3)
	stfd	f22, 68(r3)
	stfd	f23, 76(r3)
	stfd	f24, 84(r3)
	stfd	f25, 92(r3)
	stfd	f26, 100(r3)
	stfd	f27, 108(r3)
	stfd	f28, 116(r3)
	stfd	f29, 124(r3)
	stfd	f30, 132(r3)
	stfd	f31, 140(r3)

	blr

CENTRY(RestoreFPRegs)
	lfd	f14, 0(r3)
	lfd	f15, 8(r3)
	lfd	f16, 16(r3)
	lfd	f17, 24(r3)
	lfd	f18, 32(r3)
	lfd	f19, 40(r3)
	lfd	f20, 48(r3)
	lfd	f21, 56(r3)
	lfd	f22, 64(r3)
	lfd	f23, 72(r3)
	lfd	f24, 80(r3)
	lfd	f25, 88(r3)
	lfd	f26, 96(r3)
	lfd	f27, 104(r3)
	lfd	f28, 112(r3)
	lfd	f29, 120(r3)
	lfd	f30, 128(r3)
	lfd	f31, 136(r3)
	blr

#if (defined(TARGET_PPC) && (defined(OPSYS_LINUX) || defined(OPSYS_DARWIN) ))

#define CACHE_LINE_SZB		32
#define CACHE_LINE_MASK		(CACHE_LINE_SZB-1)
#define CACHE_LINE_BITS		26

/* FlushICache:
 *
 *   void FlushICache (Addr_t addr, Addr_t nbytes)
 */
CENTRY(FlushICache)
	add	r4,r3,r4		/* stop := addr+nbytes */
	addic	r4,r4,CACHE_LINE_MASK	/* stop := stop + CACHE_LINE_MASK */
	rlwinm	r4,r4,0,0,CACHE_LINE_BITS	/* stop := stop & ~CACHE_LINE_MASK */
L_FlushICache_1:
	cmplw	cr1,r3,r4		/* while (addr < stop) */
	bc	4,4,L_FlushICache_2
	dcbf	0,r3			/*   flush addr */
	icbi	0,r3			/*   invalidate addr */
	addi	r3,r3,CACHE_LINE_SZB	/*   addr := addr + CACHE_LINE_SZB */
	b	L_FlushICache_1		/* end while */
L_FlushICache_2:
	blr

#endif

