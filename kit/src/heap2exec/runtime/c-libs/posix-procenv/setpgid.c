/* setpgid.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_ProcEnv_setpgid: int * int -> unit
 *
 * Set user id
 */
ml_val_t _ml_P_ProcEnv_setpgid (ml_state_t *msp, ml_val_t arg)
{
    int         sts;

    sts = setpgid(REC_SELINT(arg, 0),REC_SELINT(arg, 1));

    CHK_RETURN_UNIT(msp, sts)

} /* end of _ml_P_ProcEnv_setpgid */

