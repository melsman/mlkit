/* kill.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <signal.h>

/* _ml_P_Process_kill : int * int -> unit
 *
 * Send a signal to a process or a group of processes
 */
ml_val_t _ml_P_Process_kill (ml_state_t *msp, ml_val_t arg)
{
    int       sts;

    sts = kill(REC_SELINT(arg, 0),REC_SELINT(arg, 1));

    CHK_RETURN_UNIT (msp, sts)

} /* end of _ml_P_Process_kill */
