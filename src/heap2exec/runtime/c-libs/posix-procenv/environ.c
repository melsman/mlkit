/* environ.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"

/* _ml_P_ProcEnv_environ : unit -> string list
 */
ml_val_t _ml_P_ProcEnv_environ (ml_state_t *msp, ml_val_t arg)
{
    extern char         **environ;

    return ML_CStringList (msp, environ);

} /* end of _ml_P_ProcEnv_environ */

