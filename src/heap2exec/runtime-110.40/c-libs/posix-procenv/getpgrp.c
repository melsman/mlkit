/* getpgrp.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_ProcEnv_getpgrp: unit -> int
 *
 * Return process group
 */
ml_val_t _ml_P_ProcEnv_getpgrp (ml_state_t *msp, ml_val_t arg)
{
    return INT_CtoML(getpgrp());

} /* end of _ml_P_ProcEnv_getpgrp */

