/* getsigmask.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-signals.h"
#include "cfun-proto-list.h"

/* _ml_Sig_getsigmask : unit -> sysconst list option
 *
 */
ml_val_t _ml_Sig_getsigmask (ml_state_t *msp, ml_val_t arg)
{
    return GetSignalMask (msp);

} /* end of _ml_Sig_getsigmask */

