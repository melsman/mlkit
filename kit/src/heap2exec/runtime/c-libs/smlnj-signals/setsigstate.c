/* setsigstate.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "ml-signals.h"
#include "cfun-proto-list.h"

/* _ml_Sig_setsigstate : (sysconst * int) -> unit
 *
 */
ml_val_t _ml_Sig_setsigstate (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	sig = REC_SEL(arg, 0);

    SetSignalState (msp->ml_vproc, REC_SELINT(sig, 0), REC_SELINT(arg, 1));

    return ML_unit;

} /* end of _ml_Sig_setsigstate */

