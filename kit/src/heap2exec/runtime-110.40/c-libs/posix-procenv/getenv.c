/* getenv.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"
#include <stdio.h>

/* _ml_P_ProcEnv_getenv: string -> string option
 *
 * Return value for environment name
 */
ml_val_t _ml_P_ProcEnv_getenv (ml_state_t *msp, ml_val_t arg)
{
    char     *sts;
    ml_val_t r, s;

    sts = getenv(STR_MLtoC(arg));
    if (sts == NIL(char *))
        r = OPTION_NONE;
    else {
        s = ML_CString(msp,sts);
        OPTION_SOME(msp, r, s)
    }
  
    return r;

} /* end of _ml_P_ProcEnv_getenv */

