/* close.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "ml-base.h"
#include "ml-values.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_Sock_close : sock -> unit
 */
ml_val_t _ml_Sock_close (ml_state_t *msp, ml_val_t arg)
{
    int		status, fd = INT_MLtoC(arg);

    status = close(fd);

    CHK_RETURN_UNIT(msp, status);

} /* end of _ml_Sock_close */
