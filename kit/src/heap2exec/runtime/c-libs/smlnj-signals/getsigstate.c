/* getsigstate.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "ml-signals.h"
#include "cfun-proto-list.h"

/* _ml_Sig_getsigstate : sysconst -> int
 *
 */
ml_val_t _ml_Sig_getsigstate (ml_state_t *msp, ml_val_t arg)
{
    int		state = GetSignalState (msp->ml_vproc, REC_SELINT(arg, 0));

    return INT_CtoML(state);

} /* end of _ml_Sig_getsigstate */

