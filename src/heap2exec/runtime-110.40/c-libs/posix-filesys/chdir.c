/* chdir.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <unistd.h>


/* _ml_P_FileSys_chdir : string -> unit
 *
 * Change working directory
 */
ml_val_t _ml_P_FileSys_chdir (ml_state_t *msp, ml_val_t arg)
{
    int		sts;

    sts = chdir(STR_MLtoC(arg));

    CHK_RETURN_UNIT(msp, sts)

} /* end of _ml_P_FileSys_chdir */
