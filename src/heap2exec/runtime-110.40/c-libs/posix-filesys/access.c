/* access.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include INCLUDE_TYPES_H
#include <sys/stat.h>
#include "ml-base.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"


/* _ml_P_FileSys_access : (string * word) -> bool
 *                         name     access_mode
 *
 * Determine accessibility of a file.
 */
ml_val_t _ml_P_FileSys_access (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	    path = REC_SEL(arg, 0);
    mode_t	    mode = REC_SELWORD(arg, 1);
    int		    sts;

    sts = access (STR_MLtoC(path), mode);

    if (sts == 0)
        return ML_true;
    else
        return ML_false;

} /* end of _ml_P_FileSys_access */
