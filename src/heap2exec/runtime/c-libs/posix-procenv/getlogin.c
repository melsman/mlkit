/* getlogin.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_ProcEnv_getlogin: unit -> string
 *
 * Return login name
 */
ml_val_t _ml_P_ProcEnv_getlogin (ml_state_t *msp, ml_val_t arg)
{
    char*     name;

    name = getlogin();
    if (name == NIL(char *))
        return RAISE_ERROR(msp, "no login name");
  
    return ML_CString (msp, name);

} /* end of _ml_P_ProcEnv_getlogin */

