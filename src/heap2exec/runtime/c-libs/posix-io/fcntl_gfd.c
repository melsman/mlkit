/* fcntl_gfd.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <fcntl.h>
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_IO_fcntl_gfd : int -> word
 *
 * Get the close-on-exec flag associated with the file descriptor.
 */
ml_val_t _ml_P_IO_fcntl_gfd (ml_state_t *msp, ml_val_t arg)
{
    int             flag;
    ml_val_t        v;

    flag = fcntl(INT_MLtoC(arg), F_GETFD);

    if (flag == -1)
        return RAISE_SYSERR(msp, flag);
    else {
        WORD_ALLOC (msp, v, flag);
        return v;
    }

} /* end of _ml_P_IO_fcntl_gfd */
