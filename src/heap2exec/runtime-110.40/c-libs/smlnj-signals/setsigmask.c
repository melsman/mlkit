/* setsigmask.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-signals.h"
#include "cfun-proto-list.h"

/* _ml_Sig_setsigmask : sysconst list option -> unit
 *
 * Mask the listed signals.
 */
ml_val_t _ml_Sig_setsigmask (ml_state_t *msp, ml_val_t arg)
{
    SetSignalMask (arg);

    return ML_unit;

} /* end of _ml_Sig_setsigmask */

