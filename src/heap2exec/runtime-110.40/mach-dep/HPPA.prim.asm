/* HPPA.prim.asm
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "asm-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-request.h"
#include "ml-limits.h"
#include "mlstate-offsets.h"	/** machine generated file **/

/* stack layout when executing in ML code. */

/* 
Note: stack grows from low to high memory addresses.

low address ...
	sp-116:		|     spill area      |
			+---------------------+	
	sp-112:		|     $$umul	      |
       			+---------------------+
	sp-44:		|      %r2-%r18       |
			+---------------------+
	sp-40:		|     *ml_state	      |
			+---------------------+
	sp-32:		|     unused[2]       |
			+---------------------+
	sp-28:		|      startgc	      |	 		
			+---------------------+
	sp-24:		|      $$mul	      |
			+---------------------+
	sp-20:		|      $$div	      |
			+---------------------+
	sp-16:		|      $$udiv	      |
			+---------------------+
	sp-12:		|     cvti2dTmp	      |
			+---------------------+
	sp-8:		|     float64Tmp      |
			+---------------------+
	sp:	
high addresses ...
	
 */

#define UDIV_OFFSET		-16
#define DIV_OFFSET		-20
#define MUL_OFFSET		-24
#define STARTGC_OFFSET  	-28
#define MLSTATE_OFFSET		-40
#define REGSAVE_OFFSET		-44
#define UMUL_OFFSET		-112
#define ML_FRAMESIZE		4096


/* Note:
		Use of the SavedStackPtr location
	 	  	  Lal George
			   12/13/95

   The SavedStackPtr location is used to restore the value of the stack
   pointer so that the layout above can be accessed. This is particularly
   relevant when an exception/trap is generated inside ml_mul, ml_div
   and ml_udiv millicode. Registers that are trashed by the system 
   millicode are saved on top of the stack together with a  
   return address.  When an exception occurs, the exception trap
   handler forces the program counter to resume at the address 
   corresponding to request_fault. However, because of the extra stuff
   on the stack, request_fault does not see the layout it expects!
   The value in SavedStackPtr is used to restore the correct stack pointer.
   The ML registers that were saved on the stack during the millicode 
   call, are ones that are not required to resume the trap handler
   and can therefore be dropped on the floor.

   The value of the stack pointer is saved before branching into ML via
   restoreregs, and is restored in saveregs and set_request---the two entry
   points into C.

   This scheme adds 6 extra instructions to go from ML to C and back.
*/

#define zero 		%r0
#define miscreg0 	%r1
#define allocptr 	%r3
#define limitptr 	%r4
#define storeptr 	%r5
#define exnptr 		%r6
#define varptr 		%r7
#define stdlink 	%r9
#define stdclos		%r10
#define stdarg 		%r11
#define stdcont 	%r12
#define miscreg1 	%r13
#define miscreg2 	%r14

#define sp 		%r30
#define pc		%r31

#define    tmp1 %r29
#define    tmp2 %r24
#define    tmp3 %r25
#define    tmp4 %r23
#define   carg0 %r26
#define creturn %r28

#define RSHIFT(r,n,t)     extrs      r, 31-(n), 32-(n), t
#define LSHIFT(r,n,t)     zdep       r, 31-(n), 32-(n), t

#define LARGECONST(c, t) ldil L%c, t ! ldo R%c(t), t


#define CONTINUE        \
	bv,n		zero(stdcont)

#define CHECKLIMIT(name)						\
	combt,<=,n      allocptr, limitptr, CSYM(CONCAT(L$$, name))	!\
	copy		stdlink, pc					!\
	b,n		saveregs1					!\
	nop							        !\
        .label          CSYM(CONCAT(L$$, name))


	/******************************************************
 	   All code must be in the data segment, since we 
	   cannot distinguish between a code and data segment 
	   offset.
	 ******************************************************/

	.data

SavedStackPtr	.word     0

ML_CODE_HDR(sigh_return_a)
	ldi	REQ_SIG_RETURN, tmp2
	ldi	0+ML_unit, stdlink
 	ldi	0+ML_unit, stdclos
	ldi	0+ML_unit, pc
	b,n	set_request

ENTRY(sigh_resume)
	ldi	REQ_SIG_RESUME, tmp2
	b,n	set_request


ML_CODE_HDR(pollh_return_a)
	ldi	REQ_POLL_RETURN, tmp2
	ldi	0+ML_unit, stdlink
 	ldi	0+ML_unit, stdclos
	ldi	0+ML_unit, pc
	b,n	set_request


ENTRY(pollh_resume)
	ldi	REQ_POLL_RESUME, tmp2
	b,n	set_request


ML_CODE_HDR(handle_a)
	ldi	REQ_EXN, tmp2
	copy	stdlink, pc
	b,n	set_request

ML_CODE_HDR(return_a)
	ldi	REQ_RETURN, tmp2
	ldi	0+ML_unit, stdlink
 	ldi	0+ML_unit, stdclos
	ldi	0+ML_unit, pc
	b,n	set_request

ENTRY(request_fault)
	ldi	REQ_FAULT, tmp2
	copy	stdlink, pc
	b,n	set_request

ML_CODE_HDR(bind_cfun_a)
	CHECKLIMIT(bind_cfun_check)
	ldi	REQ_BIND_CFUN, tmp2
	b,n	set_request

ML_CODE_HDR(build_literals_a)
	CHECKLIMIT(build_literals_check)
	ldi	REQ_BUILD_LITERALS, tmp2
	b,n	set_request

ML_CODE_HDR(callc_a)
	CHECKLIMIT(callc_check)
	ldi	REQ_CALLC, tmp2
	b,n	set_request

/*
   There are two entry points for saveregs --- saveregs0 and saveregs1.

   Saveregs0 is called from inside ML to invoke a gc. This is
   done using a BLE,n  instruction. The return address (in pc) with
   nullification set, is at the wrong place unless one puts a NOP after 
   the BLR,n. Saveregs0 is used to correct the off-by-four value in pc 
   or %r31.

   Saveregs1 is called internally (or everywhere else) where the return
   address is standard link (stdlink) typically and needs no correction.

*/
	.export saveregs0,ENTRY
ENTRY(saveregs0)
	addi	0-4, pc, pc
	ldi	0-4, tmp2
	and	pc, tmp2, pc
saveregs1
	ldi	REQ_GC, tmp2
	/* fall through */


set_request
	ldil    L%SavedStackPtr, tmp1
	ldo     R%SavedStackPtr(tmp1), tmp1	
        ldw     0(tmp1), sp			/* restore stack pointer */

	ldw	MLSTATE_OFFSET(sp), tmp1
	ldw     VProcOffMSP(tmp1), tmp4		/* use tmp4 as VProc ptr */
	stw	zero, InMLOffVSP(tmp4)		/* leaving ML */
	stw	allocptr, AllocPtrOffMSP(tmp1)
	stw	limitptr, LimitPtrOffMSP(tmp1)
	stw	storeptr, StorePtrOffMSP(tmp1)
	stw	stdlink, LinkRegOffMSP(tmp1)
	stw	pc, PCOffMSP(tmp1)		/* address of called function */
	stw	stdarg, StdArgOffMSP(tmp1)
	stw	stdclos, StdClosOffMSP(tmp1)
	stw	stdcont, StdContOffMSP(tmp1)
	stw	varptr, VarPtrOffMSP(tmp1)
	stw	exnptr, ExnPtrOffMSP(tmp1)
	copy	tmp2, creturn 			/* return request */
	stw	miscreg0,Misc0OffMSP(tmp1)
	stw	miscreg1,Misc1OffMSP(tmp1)
	stw	miscreg2,Misc2OffMSP(tmp1)
						/* fall through */
restore_c_regs
	ldw	REGSAVE_OFFSET(sp), %r2
	ldw	REGSAVE_OFFSET-4(sp), %r3
	ldw	REGSAVE_OFFSET-8(sp), %r4
        ldw     REGSAVE_OFFSET-12(sp), %r5
        ldw     REGSAVE_OFFSET-16(sp), %r6
        ldw     REGSAVE_OFFSET-20(sp), %r7
        ldw     REGSAVE_OFFSET-24(sp), %r8
        ldw     REGSAVE_OFFSET-28(sp), %r9
        ldw     REGSAVE_OFFSET-32(sp), %r10
        ldw     REGSAVE_OFFSET-36(sp), %r11
        ldw     REGSAVE_OFFSET-40(sp), %r12
        ldw     REGSAVE_OFFSET-44(sp), %r13
        ldw     REGSAVE_OFFSET-48(sp), %r14
        ldw     REGSAVE_OFFSET-52(sp), %r15
        ldw     REGSAVE_OFFSET-56(sp), %r16
        ldw     REGSAVE_OFFSET-60(sp), %r17
        ldw     REGSAVE_OFFSET-64(sp), %r18
	LARGECONST(-ML_FRAMESIZE, tmp3)
	add	tmp3, sp, sp		         /* discard the stack frame */ 
	ldsid	(%r2), tmp1 
	mtsp	tmp1, %sr1 
	be,n	0(%sr1, %r2)

/* We need to find a way of creating a table of these constant
 * values, rather than computing them each time around.
 */
#define STORE_CODE_ADDR(proc, offset)	\
	ldil	L%proc, tmp2		!\
	ldo	R%proc(tmp2), tmp2	!\
	stw	tmp2, offset(sp)

BEGIN_PROC(restoreregs)
	.export restoreregs,ENTRY
restoreregs
	LARGECONST(ML_FRAMESIZE, tmp3)
	add     tmp3, sp, sp

	ldil	L%SavedStackPtr, tmp1		/* save stack to restore */
	ldo	R%SavedStackPtr(tmp1), tmp1
	stw	sp, 0(tmp1)

	/* save the C registers */
	stw	%r2,  REGSAVE_OFFSET(sp)
	stw	%r3,  REGSAVE_OFFSET-4(sp)
	stw	%r4,  REGSAVE_OFFSET-8(sp)  
        stw     %r5,  REGSAVE_OFFSET-12(sp)  
        stw     %r6,  REGSAVE_OFFSET-16(sp)  
        stw     %r7,  REGSAVE_OFFSET-20(sp)  
        stw     %r8,  REGSAVE_OFFSET-24(sp)  
        stw     %r9,  REGSAVE_OFFSET-28(sp)  
        stw     %r10, REGSAVE_OFFSET-32(sp) 
        stw     %r11, REGSAVE_OFFSET-36(sp) 
        stw     %r12, REGSAVE_OFFSET-40(sp) 
        stw     %r13, REGSAVE_OFFSET-44(sp) 
        stw     %r14, REGSAVE_OFFSET-48(sp) 
        stw     %r15, REGSAVE_OFFSET-52(sp) 
        stw     %r16, REGSAVE_OFFSET-56(sp) 
        stw     %r17, REGSAVE_OFFSET-60(sp) 
        stw     %r18, REGSAVE_OFFSET-64(sp) 

	/* create ML stack frame */
        stw     carg0, MLSTATE_OFFSET(sp)
        copy    carg0, tmp1

	STORE_CODE_ADDR(ml_udiv, UDIV_OFFSET)
	STORE_CODE_ADDR(ml_div, DIV_OFFSET)
	STORE_CODE_ADDR(ml_mul, MUL_OFFSET)
	STORE_CODE_ADDR(ml_umul, UMUL_OFFSET)
	STORE_CODE_ADDR(saveregs0, STARTGC_OFFSET)

	ldw	AllocPtrOffMSP(tmp1), allocptr
	ldw	LimitPtrOffMSP(tmp1), limitptr
	ldw	StorePtrOffMSP(tmp1), storeptr
	ldi	1, tmp2
	ldw     VProcOffMSP(tmp1), tmp4
	stw	tmp2,InMLOffVSP(tmp4)		 /* entering ML code */
	ldw	StdArgOffMSP(tmp1), stdarg
	ldw	StdContOffMSP(tmp1), stdcont
	ldw	StdClosOffMSP(tmp1), stdclos
	ldw	ExnPtrOffMSP(tmp1), exnptr
	ldw	Misc0OffMSP(tmp1), miscreg0
	ldw	Misc1OffMSP(tmp1), miscreg1
	ldw	Misc2OffMSP(tmp1), miscreg2
	ldw	LinkRegOffMSP(tmp1), stdlink
	ldw	VarPtrOffMSP(tmp1), varptr
	ldw	PCOffMSP(tmp1), pc
						/* check for pending signals */
	ldw	NPendingSysOffVSP(tmp4), tmp2
	ldw	NPendingOffVSP(tmp4), tmp3
	add	tmp2, tmp3, tmp2
        combf,= tmp2, zero, pending_sigs
	nop				

ml_go
	mfsp	%sr5, tmp2  	  	       /* for indexed loads */
	mtsp    tmp2, %sr3
	/* The pc is used to compute the baseptr on return
	 * to ML. The privelege level bits (30 and 31) need to be 
	 * zeroed out before making the call.
	 */
	ldi	0-4, tmp2	
	and	pc, tmp2, pc
	bv,n	0(pc)

pending_sigs
	/* there are pending signals */
	/* check if signals are masked */
	ldw	InSigHandlerOffVSP(tmp4), tmp2
	combf,= tmp2, zero, ml_go
	nop
	
	/* note that a handler trap is pending */
	ldi	1, tmp2
	stw	tmp2, HandlerPendingOffVSP(tmp4)
	copy	limitptr,allocptr	
	b,n	ml_go
END_PROC(restoreregs)


ENTRY(SaveFPRegs)
	break 0,0			/* should never be called */

ENTRY(RestoreFPRegs)
	break 0,0			/* should never be called */

/*
 * array : (int * 'a) -> 'a array
 */
ML_CODE_HDR(array_a)
	CHECKLIMIT(array_check)

	ldw	0(stdarg), tmp1		    /* tmp1 := length (tagged int) */
        RSHIFT(tmp1, 1, tmp2)		    /* tmp2 := length : untagged int */
	ldi     SMALL_OBJ_SZW, tmp3	    /* is this a small object */
	combt,< tmp3, tmp1, L$array_offline
	nop

	/* allocate and initialize array data */
	ldw	4(stdarg), stdarg	    /* stdarg = initial value */
	LSHIFT(tmp2, TAG_SHIFTW, tmp3)	    /* build descriptor in tmp3 */
	ldi	0+MAKE_TAG(DTAG_arr_data), tmp4
	or	tmp3, tmp4, tmp3
	stw	tmp3,0(allocptr)	    /* store descriptor */
	addi    4, allocptr, allocptr	    /* allocptr++ */
	copy 	allocptr, tmp3		    /* array data ptr in tmp3 */

	LSHIFT(tmp2, 2, tmp2)		    /* tmp2 = number of bytes to allocate */
	add	tmp2, allocptr, tmp2	    /* tmp2 = address of end of array  */
L$array_loop
	stw	stdarg, 0(allocptr)	    
	addi	4, allocptr, allocptr
	combf,= allocptr, tmp2, L$array_loop
	nop

	/* allocate array header */
	ldi	0+(DESC_polyarr), tmp2	    /* descriptor in tmp2 */
	stw	tmp2, 0(allocptr)	    /* store the descriptor */
	addi	4, allocptr, allocptr	    /* allocptr++ */
	copy	allocptr, stdarg	    /* result = header addr */
	stw	tmp3, 0(allocptr)	    /* store pointer to data */
	stw	tmp1, 4(allocptr)	   
	addi	8, allocptr, allocptr	    /* allocptr += 2 */
	CONTINUE
L$array_offline			/* off-line allocation of big arrays */
	ldi	REQ_ALLOC_ARRAY, tmp2
	copy	stdlink, pc
	b,n	set_request



ML_CODE_HDR(create_r_a)
	CHECKLIMIT(creat_r_check)

	RSHIFT(stdarg, 1, tmp2)		/* tmp2 = length (untagged int) */
	LSHIFT(tmp2, 1, tmp2)		/* tmp2 = length in words */
	ldi     SMALL_OBJ_SZW, tmp3
	combt,< tmp3, tmp2, L$realarray_offline
	nop

	LSHIFT(tmp2, TAG_SHIFTW, tmp1)	    /* build descriptor in tmp1 */
	ldi	0+MAKE_TAG(DTAG_raw64), tmp3
	or	tmp1, tmp3, tmp1

	ldi	4, tmp3			/* align start floating addr */
	or	allocptr, tmp3, allocptr    
	stw	tmp1, 0(allocptr)	/* store data descriptor */
	addi	4, allocptr, allocptr	/* allocptr++ */
	copy 	allocptr, tmp3		/* tmp3 = data object */
	LSHIFT(tmp2, 2, tmp2)		/* tmp2 = length in bytes */
	add	allocptr, tmp2, allocptr/* allocptr += length */

	/* allocate the header object */
	ldi	0+(DESC_real64arr), tmp1
	stw	tmp1, 0(allocptr)	/* header descriptor */
	addi	4, allocptr, allocptr	/* allocptr++ */
	stw 	tmp3, 0(allocptr)	/* header data field */
	stw	stdarg,4(allocptr)	/* header length field */
	copy	allocptr, stdarg	/* stdarg = header object */
	addi	8, allocptr, allocptr 	/* allocptr += 2 */
	CONTINUE

L$realarray_offline
	/* off-line allocation of big realarrays */
	ldi	REQ_ALLOC_REALDARRAY, tmp2
	copy	stdlink, pc
	b,n	set_request



ML_CODE_HDR(create_b_a)
	CHECKLIMIT(create_b_checked)

	RSHIFT(stdarg, 1, tmp2)		    /* tmp2 := length (untagged) */
	addi	3, tmp2, tmp2		    /* tmp2 := length (words) */ 
	RSHIFT(tmp2, 2, tmp2)
	ldi 	SMALL_OBJ_SZW, tmp3	    /* is this a small object? */
	combt,< tmp3, tmp2, L$bytearray_offline /* no */
	nop

	/* allocate the data object */
	LSHIFT(tmp2, TAG_SHIFTW, tmp1)	    /* descriptor in tmp1 */
	ldi     0+MAKE_TAG(DTAG_raw32), tmp3
	or	tmp1, tmp3, tmp1
	stw 	tmp1, 0(allocptr)	    /* write out descriptor */
	addi	4, allocptr, allocptr	    /* allocptr++  */ 
	copy	allocptr, tmp3		    /* tmp3 = data object */
	LSHIFT(tmp2, 2, tmp2)		    /* length in bytes */
	add	tmp2, allocptr, allocptr    /* allocptr += length */

	/* allocate the header object */
	ldi	0+(DESC_word8arr), tmp1	    /* header descriptor */
	stw	tmp1, 0(allocptr)		
        addi	4, allocptr, allocptr	    /* allocptr++ */
	stw	tmp3, 0(allocptr)	    /* header data field */
        stw	stdarg, 4(allocptr)	    /* header length field */
        copy	allocptr, stdarg	    /* stdarg = header object */
        addi    8, allocptr, allocptr	    /* allocptr += 2 */
	CONTINUE

L$bytearray_offline			    /* big object */
	ldi	REQ_ALLOC_BYTEARRAY, tmp2
	copy	stdlink, pc
	b,n	set_request



ML_CODE_HDR(create_s_a)
	CHECKLIMIT(create_s_checked)

	RSHIFT(stdarg, 1, tmp2)		/* tmp2 := length: untagged int */
	addi	4, tmp2, tmp2		/* tmp2 := length in words */
	RSHIFT(tmp2, 2, tmp2)		
	ldi	SMALL_OBJ_SZW, tmp3	/* is this a big object */
	combt,< tmp3, tmp2, L$string_offline /* no */
        nop

	/* allocate the data object */
	LSHIFT(tmp2, TAG_SHIFTW, tmp1)	/* build descriptor in tmp1 */
	ldi	0+MAKE_TAG(DTAG_raw32), tmp3
	or	tmp1, tmp3, tmp1	
	stw	tmp1, 0(allocptr)	/* store descriptor */
	addi	4, allocptr, allocptr	/* allocptr++ */
	copy	allocptr, tmp3		/* tmp3 = data object */
	LSHIFT(tmp2, 2, tmp2)		/* length in bytes */
	add	tmp2, allocptr, allocptr/* allocptr += length */
	stw	zero, -4(allocptr)       /* zero-terminate string */

     /* allocate the header object */
	ldi	0+(DESC_string), tmp1	/* header descriptor */
	stw	tmp1, 0(allocptr)	/* header data field */
        addi	4, allocptr, allocptr	/* allocptr++ */
	stw	tmp3, 0(allocptr)	/* header data field */
	stw	stdarg, 4(allocptr)	/* header length field */
        copy	allocptr, stdarg	/* stdarg = header object */
	addi	8, allocptr, allocptr	/* allocptr += 2 */
	CONTINUE

L$string_offline
	ldi	REQ_ALLOC_STRING, tmp2
	copy	stdlink, pc
	b,n	set_request



ML_CODE_HDR(create_v_a)
	CHECKLIMIT(create_v_checked)

	ldw	0(stdarg), tmp1		/* tmp1 = tagged length */
	RSHIFT(tmp1, 1, tmp2)		/* tmp2 = untagged length */
	ldi	SMALL_OBJ_SZW, tmp3	/* is this a small object? */
	combt,< tmp3, tmp2, L$vector_offline /* no */
	nop

	/* allocate and initialize data object */
	LSHIFT(tmp2, TAG_SHIFTW, tmp2)	/* build descriptor in tmp2 */
	ldi	0+MAKE_TAG(DTAG_vec_data), tmp3
	or	tmp2, tmp3, tmp2
	stw	tmp2, 0(allocptr)	/* store descriptor */
	addi	4, allocptr, allocptr	/* allocptr++ */
	ldw	4(stdarg), tmp2		/* tmp2 = list */
	copy	allocptr, stdarg	/* stdarg = data obj */
	ldi	0+ML_nil, tmp4
L$vector_loop
	ldw	0(tmp2), tmp3		/* tmp3 = hd(tmp2) */
	ldw	4(tmp2), tmp2		/* tmp2 = tl(tmp2) */
	stw	tmp3, 0(allocptr)	/* store word in vector */
	addi	4, allocptr, allocptr	/* allocptr++ */
	combf,= tmp2, tmp4, L$vector_loop/* if (tmp2 <> nil) goto loop */
	nop

	/* allocate header object */
	ldi	0+(DESC_polyvec), tmp3	/* descriptor in tmp3 */
	stw	tmp3, 0(allocptr)	/* header descriptor */
	addi	4, allocptr, allocptr	/* allocptr++ */
	stw	stdarg, 0(allocptr)	/* header data field */
	stw	tmp1, 4(allocptr)	/* header length field */
	copy	allocptr, stdarg	/* result = header object */
	addi	8, allocptr, allocptr	/* allocptr += 2 */
	CONTINUE
L$vector_offline
	ldi	REQ_ALLOC_VECTOR, tmp2
	copy	stdlink, pc
	b,n	set_request

/* logb --- extract and unbias the exponent */
ML_CODE_HDR(logb_a)
	ldw	0(stdarg), stdarg	/* msb */
	extru   stdarg, 11, 12, stdarg  /* throw out 20 low bits */
	ldo     0x7ff(%r0), tmp1	/* retain 11 bits */
	and	stdarg, tmp1, stdarg
	ldo     0-1023(stdarg), stdarg	/* unbias */
	LSHIFT(stdarg, 1, stdarg)	/* tag as ML int */
	addi    1, stdarg, stdarg
   	CONTINUE



/* scalb(u:real,v:int) = u * 2 ^ v */
ML_CODE_HDR(scalb_a)
	CHECKLIMIT(scalb_a_checked)
	ldw	4(stdarg),tmp1		/* tmp1 := v tagged */
	RSHIFT(tmp1, 1, tmp1)		/* tmp1 := v */
	ldw	0(stdarg),stdarg	/* stdarg := u */
	ldw     0(stdarg), tmp2		/* tmp2 := MSW(u) */
	ldil	L%0x7ff00000, tmp3	/* mask */
	and	tmp2, tmp3, tmp3	/* tmp3 := tmp2 & 0x7ff00000 */
	combt,=,n tmp3, %r0, scalb_all_done /* u == 0.0 */

	RSHIFT(tmp3, 20, tmp3)		/* tmp3 := ieee(exp) */
	addo	tmp3, tmp1, tmp3	/* tmp3 := scaled exponent */
	combt,<,n	tmp3, %r0, scalb_underflow

	ldi	2047, tmp1		/* max. ieee(exp) */
	combt,<,n tmp1, tmp3, scalb_overflow
	
	ldil	L%0x800fffff, tmp1	/* tmp1 := sign bit + mantissa mask */
	ldo	R%0x800fffff(tmp1), tmp1
	and	tmp1, tmp2, tmp1	/* tmp1 := original sign and mantessa*/
	LSHIFT(tmp3, 20, tmp3)		/* tmp3 := exp in right place*/
	or	tmp1, tmp3, tmp1	/* tmp1 := MSW(u) */
	ldw	4(stdarg), tmp2		/* tmp2 := LSW(u) */
	/* fall through */

scalb_write_out				/* {tmp1, tmp2} live on entry */
	ldi	4, tmp3			/* align allocation pointer */
	or      tmp3, allocptr, allocptr     
	stw	tmp1, 4(allocptr)	/* store MSW */
	stw	tmp2, 8(allocptr)	/* store LSW */
	ldi	0+(DESC_reald),tmp3	/* store descriptor */
	stw	tmp3,0(allocptr)
	addi	0x4,allocptr,stdarg	/* return pointer to float */
	addi	0xc,allocptr,allocptr	/* bump allocation pointer */
	/* fall through */

scalb_all_done
	/* BUG: The compiler supports arithmetic over denormalized
	 *  numbers, but  scalb barfs at them. Denormalized numbers 
	 *  are treated here as 0.0.
	 */
   	CONTINUE

scalb_underflow
	/* BUG: Incorrect behaviour on underflow, should return the
	 *  denormalized number.
  	 */
	ldi	0, tmp1
	ldi	0, tmp2
	b,n	scalb_write_out	

scalb_overflow
	ldil	L%0x7fffffff,tmp1
	ldo	R%0x7fffffff(tmp1),tmp1
	addo	tmp1,tmp1,0			/* generate trap */
	/* should never execute the next instruction */


floor_MAXINT	.double 1073741824.0

ML_CODE_HDR(floor_a)
	fldds   0(stdarg), %fr4			/* fr4 := argument */

    	ldi	0x60e, tmp1		/* set rounding mode to -inf */
    	stw	tmp1, 0-4(sp)		/* store in temp scratch */
    	fldws	0-4(sp), %fr0L
    	fcnvfx,dbl,sgl %fr4, %fr4R 

	stw	zero,0-4(sp)
    	fldws	0-4(sp),%fr0L
    	fstws	%fr4R,0-4(sp)
    	ldw	0-4(sp), stdarg
    	add	stdarg, stdarg, stdarg
    	ldo	1(stdarg), stdarg
    	CONTINUE


/* try_lock_a */
ML_CODE_HDR(try_lock_a)
	CHECKLIMIT(try_lock_check)
	ldw	0(stdarg), tmp1
	ldi	0+ML_true, tmp2
	stw	tmp2, 0(stdarg)
	copy	tmp2, stdarg
	CONTINUE


ML_CODE_HDR(unlock_a)
	CHECKLIMIT(unlock_check)
	ldi	0+ML_false, tmp1
	stw	tmp1, 0(stdarg)
	ldi	0+ML_unit, stdarg
	CONTINUE




		/* milli code routines */

/* 
  millicode:
  inputs in %r26 (arg0) and %r25 (arg1)
  result in %r29 (ret1)
  
  saved: %r25, %r26, %r1   --- trashed by millicode routines
         %r31	           --- trashed by BLE

  Note: If the millicode were inlined in this data segment then it would
  not be necessary to do this cross-segment jump.
*/

/* Note: Offset -20(sp) is used by DoMillicode */
#define MILLI_LOCAL_AREA	24	/* multiple of 8 */
#define millicodeSave				\
	addi	MILLI_LOCAL_AREA, sp, sp	!\
	stw	%r1, 0-4(sp)			!\
	stw	%r25,0-8(sp)			!\
	stw	%r26,0-12(sp)			!\
	stw	%r31,0-16(sp)

#define millicodeRestore			\
	ldw	0-16(sp), %r31			!\
	ldw	0-12(sp), %r26			!\
	ldw	0-8(sp), %r25			!\
	ldw	0-4(sp), %r1			!\
	addi	0-MILLI_LOCAL_AREA, sp, sp	!\
	addi    0-4, pc, pc		!\
	bv,n	0(pc)
	
#define InvokeMillicode(proc)		 \
	millicodeSave			!\
	ldil	L%proc, %r1		!\
	ldo	R%proc(%r1), %r1	!\
	ldsid	(%r1), %r29		!\
	mtsp	%r29, %sr1		!\
	ble,n   0(%sr1, %r1)		!\
	nop				!\
	millicodeRestore

	.export ml_mul,ENTRY
	.export ml_umul,ENTRY
	.export ml_div,ENTRY
	.export ml_udiv,ENTRY
	
floatingZero	.double 	0.0
floatingOne	.double		1.0

/* The  bogus addit,= below is to cause an immediate trap */
#define divByZeroCheck(lab)				\
	combf,=	%r0, %r25, lab				!\
	nop						!\
	ldil	L%floatingZero, %r29			!\
	ldo		R%floatingZero(%r29), %r29	!\
	fldds	0(%r29), %fr4				!\
	ldil	L%floatingOne, %r29			!\
	ldo		R%floatingOne(%r29), %r29	!\
	fldds	0(%r29), %fr5				!\
	fdiv,dbl	%fr5, %fr4, %fr4		!\
	fstds	%fr4, 0(sp)				!\
	.label lab
		
ENTRY(ml_mul)
	InvokeMillicode(do_mulI)
ENTRY(ml_umul)
	InvokeMillicode(do_mulU)
ENTRY(ml_udiv)
	divByZeroCheck(noUdivByZero)
	InvokeMillicode(do_divU)
	
ENTRY(ml_div)
	divByZeroCheck(noDivByZero)
	comibf,= 	0-1, %r25, mlDivNoOverflow
	nop
	ldo	0x1, tmp1
	subo	%r26, tmp1, tmp1
mlDivNoOverflow
	InvokeMillicode(do_divI)

/*----------------------------------------------------------------*/
		.code

#define DoMillicode(proc)	\
    stw		%r31, 0-20(sp)	!\
    bl,n	proc, %r31	!\
    nop				!\
    ldw		0-20(sp), %r31	!\
    ldsid	(%r31), %r1	!\
    mtsp	%r1, %sr1	!\
    be,n 	0(%sr1, %r31)

	.import $$divI,MILLICODE
	.import $$divU,MILLICODE
	.import $$muloI,MILLICODE
	.import $$mulU,MILLICODE
		
	.export do_mulI,ENTRY
	.export do_mulU,ENTRY
	.export do_divI,ENTRY
	.export do_divU,ENTRY

do_mulI
	DoMillicode($$muloI)
do_mulU
	DoMillicode($$mulU)
do_divI
	DoMillicode($$divI)
do_divU
	DoMillicode($$divU)

		
    .export FlushICache,ENTRY
FlushICache
	.proc
	.callinfo

	.enter
	ldsid	(26), 23	/* get space id from short pointer */
	mtsp	23, 2		/* stick it in scratch space reg */

	depi	0,31,4,26	/* align address to cache line */
	addi	15,25,25	/* align size upwards */
	depi	0,31,4,25		    
	ldi	16,22		/* r22 := minimum cache line size */
	ldi	-16,21		/* r21 := -(minimum cache line size) */

fic_loop
	fdc	0(2,26)
	sync
	/* fic can't use short pointer so 
         * use the space reg set up above
	 */	
	fic,m	22(2,26)	

	nop			/* 7 cycle delay. See programming note */
	nop			/* for SYNC in arch. ref. manual */
	nop 
	nop 
	nop 
	nop 
	nop 

	addb,>=	21,25,fic_loop	/* add stride to count, branch */
	nop 
       .leave 
       .procend




/* set_fsr - set IEEE floating point enables. */
/* saving and restoring tmp1 is temporary paranoia */

	.export set_fsr,ENTRY
set_fsr
	.proc
	.callinfo FRAME=64
	.enter 
	stw	zero,0-4(sp)
	fldws	0-4(sp),%fr0L
	.leave
	.procend

	.export pointer2space
pointer2space
	.proc
	.callinfo
	.entry
	bv	0(2)
	ldsid	(26), 28
	.leave
	.procend

	.end			; End of program
