/* chmod.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <sys/types.h>
#include <sys/stat.h>

/* _ml_P_FileSys_chmod : (string * word) -> unit
 *                        name     mode
 *
 * Change mode of file
 */
ml_val_t _ml_P_FileSys_chmod (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	    path = REC_SEL(arg, 0);
    mode_t	    mode = REC_SELWORD(arg, 1);
    int		    sts;

    sts = chmod (STR_MLtoC(path), mode);

    CHK_RETURN_UNIT(msp, sts)

} /* end of _ml_P_FileSys_chmod */
