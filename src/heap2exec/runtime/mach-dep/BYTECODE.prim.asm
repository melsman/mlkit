/* BYTECODE.prim.asm
 */

#include "ml-base.h"
#include "asm-base.h"
#include "tags.h"
#include "ml-request.h"
#include "bc-instrs.h"
#include "reg-mask.h"

/* assumes that CALLESAVE > 0 */
#define CONTINUE                                \
            I_JMPind(r1)


	RDATA

	GLOBAL(asm_start)
LABEL(CSYM(asm_start))

/* sigh_return_a:
 * The return continuation for the ML signal handler.
 */
ML_CODE_HDR(sigh_return_a)
	I_CALLC(REQ_SIG_RETURN)

/* sigh_resume:
 * Resume execution at the point at which a handler trap occurred.  This is a
 * standard two-argument function, thus the closure is in ml_cont (a1).
 */
GLOBAL(sigh_resume)
LABEL(sigh_resume)
	I_CALLC(REQ_SIG_RESUME)

ML_CODE_HDR(handle_a)
        I_CALLC(REQ_EXN)

ML_CODE_HDR(return_a)
        I_CALLC(REQ_RETURN)

ML_CODE_HDR(build_literals_a)
	I_LIMITCHK(0,FUN_MASK)		/* in case there is a pending signal */
        I_CALLC(REQ_BUILD_LITERALS)

ML_CODE_HDR(callc_a)
	I_LIMITCHK(0,FUN_MASK)		/* in case there is a pending signal */
	I_CALLC(REQ_CALLC)

/* array : (int * 'a) -> 'a array
 * Allocate and initialize a new array.  This can cause GC.
 */
ML_CODE_HDR(array_a)
	I_ARRAY
	CONTINUE

/* create_v : int * 'a list -> 'a vector
 * Creates a vector with elements taken from a list.
 * n.b. The front end ensure that list cannot be nil.
 */
ML_CODE_HDR(create_v_a)
	I_VECTOR
	CONTINUE

/* create_s : int -> string
 * Create a string of given length.  This can cause GC.
 */
ML_CODE_HDR(create_s_a)
	I_STRING
	CONTINUE

/* create_b : int -> bytearray
 * Create a bytearray of given length.  This can cause GC.
 */
ML_CODE_HDR(create_b_a)
	I_BYTEARRAY
	CONTINUE

/* create_r : int -> realarray
 * Create a realarray of given length.  This can cause GC.
 */
ML_CODE_HDR(create_r_a)
	I_REALARRAY
	CONTINUE

/* try_lock: spin_lock -> bool.
 * low-level test-and-set style primitive for mutual-exclusion among
 * processors.  For now, we only provide a uni-processor trivial version.
 */
ML_CODE_HDR(try_lock_a)
	CONTINUE

/* unlock : releases a spin lock
 */
ML_CODE_HDR(unlock_a)
	CONTINUE

/* floor : real -> int
 * Return the floor of the argument or else raise Float("floor") if out of range.
 */
ML_CODE_HDR(floor_a)
	I_FLOAD(f0,r0)
	I_FLOOR(r0,f0)
	I_ASHL1(r0,r0)
	I_INCR(r0,1)
	CONTINUE

/* logb : real -> int
 * Extract and unbias the exponent, return 0 for a zero exponent.
 * The IEEE bias is 1023.
 */
ML_CODE_HDR(logb_a)
	I_FLOAD(f0,r0)
	I_LOGB(r0,f0)
	CONTINUE

/* scalb : (real * int) -> real
 * Scale the first argument by 2 raised to the second argument.	 Raise
 * Float("underflow") or Float("overflow") as appropriate.
 */
ML_CODE_HDR(scalb_a)
	I_LIMITCHK(3,FUN_MASK)
	I_LOAD(r14,r0)
	I_FLOAD(f0,r14)
	I_LOADoffset(r0,r0,4)
	I_SCALB(f0,f0,r0)
	I_FALLOC(r0,f0)
	CONTINUE

	GLOBAL(asm_end)
LABEL(CSYM(asm_end))
