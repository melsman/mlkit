/* argv.c
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

/* _ml_Proc_argv:
 */
ml_val_t _ml_Proc_argv (ml_state_t *msp, ml_val_t arg)
{
    return ML_CStringList (msp, CmdLineArgs);

} /* end of _ml_Proc_argv */

