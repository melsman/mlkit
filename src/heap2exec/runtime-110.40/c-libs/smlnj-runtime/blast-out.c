/* blast_out.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-c.h"
#include "heap-io.h"
#include "cfun-proto-list.h"

/* _ml_RunT_blast_out : 'a -> Word8Vector.vector
 *
 * Translate a heap object into a linear representation (vector of bytes).
 */
ml_val_t _ml_RunT_blast_out (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	data;

    data = BlastOut (msp, arg);

    if (data == ML_unit)
	return RAISE_ERROR(msp, "attempt to blast object failed");
    else
	return data;

} /* end of _ml_RunT_blast_out */

