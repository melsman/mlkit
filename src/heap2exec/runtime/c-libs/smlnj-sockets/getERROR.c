/* getERROR.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "sockets-osdep.h"
#include INCLUDE_TYPES_H
#include INCLUDE_SOCKET_H
#include "ml-base.h"
#include "ml-values.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_Sock_getERROR : sock -> bool
 */
ml_val_t _ml_Sock_getERROR (ml_state_t *msp, ml_val_t arg)
{
    int		sock = INT_MLtoC(arg);
    int		flg, sts, optSz = sizeof(int);

    sts = getsockopt (sock, SOL_SOCKET, SO_ERROR, (sockoptval_t)&flg, &optSz);

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);
    else
	return (flg ? ML_true : ML_false);

} /* end of _ml_Sock_getERROR */
