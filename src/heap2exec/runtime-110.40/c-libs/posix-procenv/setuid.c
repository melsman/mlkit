/* setuid.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_ProcEnv_setuid: word -> unit
 *
 * Set user id
 */
ml_val_t _ml_P_ProcEnv_setuid (ml_state_t *msp, ml_val_t arg)
{
    int         sts;

    sts = setuid(WORD_MLtoC(arg));

    CHK_RETURN_UNIT(msp, sts)

} /* end of _ml_P_ProcEnv_setuid */

