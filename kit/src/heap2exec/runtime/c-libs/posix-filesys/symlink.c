/* symlink.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <unistd.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"


/* _ml_P_FileSys_symlink : string * string -> unit
 *                         existing newname
 *
 * Creates a symbolic link from newname to existing file.
 */
ml_val_t _ml_P_FileSys_symlink (ml_state_t *msp, ml_val_t arg)
{
    int		sts;
    char        *existing = REC_SELPTR(char, arg, 0);
    char        *newname = REC_SELPTR(char, arg, 1);

    sts = symlink(existing, newname);

    CHK_RETURN_UNIT (msp, sts)

} /* end of _ml_P_FileSys_symlink */
