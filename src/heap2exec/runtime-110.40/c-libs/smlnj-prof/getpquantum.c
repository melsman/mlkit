/* getpquantum.c
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "cfun-proto-list.h"
#include "profile.h"

/* _ml_Prof_getpquantum : unit -> int
 *
 * Return the profile timer quantim in microseconds.
 */
ml_val_t _ml_Prof_getpquantum (ml_state_t *msp, ml_val_t arg)
{
    return INT_CtoML(PROFILE_QUANTUM_US);

} /* end of _ml_Prof_getpquantum */
