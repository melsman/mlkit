/* debug.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 *
 * Print a string out to the debug stream.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "cfun-proto-list.h"


/* _ml_RunT_debug : string -> unit
 *
 */
ml_val_t _ml_RunT_debug (ml_state_t *msp, ml_val_t arg)
{
    SayDebug (STR_MLtoC(arg));

    return ML_unit;

} /* end of _ml_RunT_debug */

