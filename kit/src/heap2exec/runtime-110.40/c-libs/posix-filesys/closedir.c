/* closedir.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "ml-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <sys/types.h>
#include <dirent.h>

/* _ml_P_FileSys_closedir : object -> unit
 *
 * Close a directory stream.
 */
ml_val_t _ml_P_FileSys_closedir (ml_state_t *msp, ml_val_t arg)
{
    int        sts;
    
    sts = closedir(PTR_MLtoC(DIR, arg));

    CHK_RETURN_UNIT(msp,sts)

} /* end of _ml_P_FileSys_closedir */
