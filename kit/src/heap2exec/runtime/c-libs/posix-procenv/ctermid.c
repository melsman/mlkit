/* ctermid.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <stdio.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_ProcEnv_ctermid: unit -> string
 *
 * Return pathname of controlling terminal.
 */
ml_val_t _ml_P_ProcEnv_ctermid (ml_state_t *msp, ml_val_t arg)
{
    char     name[L_ctermid];
    char     *sts;

    sts = ctermid(name);
    if (sts == NIL(char *) || *sts == '\0')
        return RAISE_ERROR(msp, "cannot determine controlling terminal");
  
    return ML_CString (msp, name);

} /* end of _ml_P_ProcEnv_ctermid */

