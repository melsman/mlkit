/* cmd-name.c
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

/* _ml_Proc_cmd_name : unit -> string
 */
ml_val_t _ml_Proc_cmd_name (ml_state_t *msp, ml_val_t arg)
{
    return ML_CString (msp, MLCmdName);

} /* end of _ml_Proc_cmd_name */

