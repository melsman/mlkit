/* pause.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "ml-signals.h"
#include "cfun-proto-list.h"

/* _ml_Sig_pause : unit -> unit
 *
 * Pause until the next signal.
 */
ml_val_t _ml_Sig_pause (ml_state_t *msp, ml_val_t arg)
{

    PauseUntilSignal (msp->ml_vproc);

    return ML_unit;

} /* end of _ml_Sig_pause */

