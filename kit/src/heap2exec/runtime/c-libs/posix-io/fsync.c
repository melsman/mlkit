/* fsync.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_IO_fsync : int -> unit
 *
 * Duplicate an open file descriptor
 */
ml_val_t _ml_P_IO_fsync (ml_state_t *msp, ml_val_t arg)
{
    int         sts, fd = INT_MLtoC(arg);

    sts = fsync(fd);

    CHK_RETURN_UNIT(msp, sts)

} /* end of _ml_P_IO_fsync */
