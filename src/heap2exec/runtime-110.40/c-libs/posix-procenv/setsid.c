/* setsid.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "ml-base.h"
#include "ml-values.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_ProcEnv_setsid: unit -> int
 *
 * Set session id
 */
ml_val_t _ml_P_ProcEnv_setsid (ml_state_t *msp, ml_val_t arg)
{
    pid_t      pid;

    pid = setsid ();

    CHK_RETURN(msp, pid)

} /* end of _ml_P_ProcEnv_setsid */

