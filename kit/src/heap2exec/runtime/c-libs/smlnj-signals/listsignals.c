/* listsignals.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-signals.h"
#include "cfun-proto-list.h"

/* _ml_Sig_listsignals : unit -> sysconst list
 *
 * List the supported signals.
 */
ml_val_t _ml_Sig_listsigs (ml_state_t *msp, ml_val_t arg)
{
    return ListSignals (msp);

} /* end of _ml_Sig_pause */

