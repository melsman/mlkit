/* setgid.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_ProcEnv_setgid: word -> unit
 *
 * Set group id
 */
ml_val_t _ml_P_ProcEnv_setgid (ml_state_t *msp, ml_val_t arg)
{
    int         sts;

    sts = setgid(WORD_MLtoC(arg));

    CHK_RETURN_UNIT(msp, sts)

} /* end of _ml_P_ProcEnv_setgid */

