/* lseek.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include INCLUDE_TYPES_H
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_IO_lseek : int * int * int -> int
 *
 * Move read/write file pointer.
 */
ml_val_t _ml_P_IO_lseek (ml_state_t *msp, ml_val_t arg)
{
    int         fd = REC_SELINT(arg, 0);
    off_t       offset = REC_SELINT(arg, 1), pos;
    int         whence = REC_SELINT(arg, 2);

    pos = lseek(fd, offset, whence);

    CHK_RETURN(msp, pos)

} /* end of _ml_P_IO_lseek */
