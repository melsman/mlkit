/* dummy.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 *
 * This is a dummy run-time routine for when we would like to call
 * a null C function.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "cfun-proto-list.h"


/* _ml_RunT_dummy : string -> unit
 *
 * The string argument can be used as a unique marker.
 */
ml_val_t _ml_RunT_dummy (ml_state_t *msp, ml_val_t arg)
{
    char	*s = PTR_MLtoC(char, arg);

    return ML_unit;

} /* end of _ml_RunT_dummy */

