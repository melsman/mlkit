/* rename.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <stdio.h>
#include <unistd.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_FileSys_rename : string * string -> unit
 *                        oldname  newname
 *
 * Change the name of a file
 */
ml_val_t _ml_P_FileSys_rename (ml_state_t *msp, ml_val_t arg)
{
    int		sts;
    ml_val_t	oldname = REC_SEL(arg, 0);
    ml_val_t	newname = REC_SEL(arg, 1);

    sts = rename(STR_MLtoC(oldname), STR_MLtoC(newname));

    CHK_RETURN_UNIT (msp, sts)

} /* end of _ml_P_FileSys_rename */
