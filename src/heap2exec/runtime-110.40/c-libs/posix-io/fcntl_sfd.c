/* fcntl_sfd.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <fcntl.h>
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_IO_fcntl_sfd : int * word -> unit
 *
 * Set the close-on-exec flag associated with the file descriptor.
 */
ml_val_t _ml_P_IO_fcntl_sfd (ml_state_t *msp, ml_val_t arg)
{
    int             sts;
    int             fd0 = REC_SELINT(arg, 0);
    Word_t          flag = REC_SELWORD(arg, 1);

    sts = fcntl(fd0, F_SETFD, flag);

    CHK_RETURN_UNIT(msp,sts)

} /* end of _ml_P_IO_fcntl_sfd */
