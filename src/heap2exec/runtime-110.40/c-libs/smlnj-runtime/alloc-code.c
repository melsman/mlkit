/* alloc-code.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 */

#include "cache-flush.h"
#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"


/* _ml_RunT_alloc_code : int -> Word8Array.array
 *
 * Allocate a code object of the given size.
 *
 * Note: Generating the name string within the code object is now
 *       part of the code generator's responsibility.
 */
ml_val_t _ml_RunT_alloc_code (ml_state_t *msp, ml_val_t arg)
{
    int		nbytes = INT_MLtoC(arg);
    ml_val_t	code, res;

    code = ML_AllocCode (msp, nbytes);

    SEQHDR_ALLOC(msp, res, DESC_word8arr, code, nbytes);

    return res;
} /* end of _ml_RunT_alloc_code */
