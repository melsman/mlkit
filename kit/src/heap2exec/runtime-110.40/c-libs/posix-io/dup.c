/* dup.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_IO_dup : int -> int
 *
 * Duplicate an open file descriptor
 */
ml_val_t _ml_P_IO_dup (ml_state_t *msp, ml_val_t arg)
{
    int             fd0 = INT_MLtoC(arg);
    int             fd1;

    fd1 = dup(fd0);

    CHK_RETURN(msp, fd1)

} /* end of _ml_P_IO_dup */
