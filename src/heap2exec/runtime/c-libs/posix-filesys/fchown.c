/* fchown.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <sys/types.h>
#include <unistd.h>

/* _ml_P_FileSys_fchown : (int * word * word) -> unit
 *                         fd     uid    gid
 *
 * Change owner and group of file given a file descriptor for it.
 */
ml_val_t _ml_P_FileSys_fchown (ml_state_t *msp, ml_val_t arg)
{
    int	            fd =  REC_SELINT (arg, 0);
    uid_t           uid = REC_SELWORD(arg, 1);
    gid_t           gid = REC_SELWORD(arg, 2);
    int		    sts;

    sts = fchown (fd, uid, gid);

    CHK_RETURN_UNIT(msp, sts)

} /* end of _ml_P_FileSys_fchown */
