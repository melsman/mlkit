/* mkliterals.c
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"


/* _ml_RunT_mkliterals : Word8Vector.vector -> object vector
 *
 */
ml_val_t _ml_RunT_mkliterals (ml_state_t *msp, ml_val_t arg)
{

    return BuildLiterals (msp, GET_SEQ_DATAPTR(Byte_t, arg), GET_SEQ_LEN(arg));

} /* end of _ml_RunT_mkcode */

