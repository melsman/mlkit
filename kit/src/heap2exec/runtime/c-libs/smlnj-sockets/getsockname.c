/* getsockname.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "sockets-osdep.h"
#include INCLUDE_TYPES_H
#include INCLUDE_SOCKET_H
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_Sock_getsockname : sock -> addr
 */
ml_val_t _ml_Sock_getsockname (ml_state_t *msp, ml_val_t arg)
{
    int		sock = INT_MLtoC(arg);
    char	addrBuf[MAX_SOCK_ADDR_SZB];
    int		addrLen = MAX_SOCK_ADDR_SZB;
    int		sts;

    sts = getsockname (sock, (struct sockaddr *)addrBuf, &addrLen);

    if (sts == -1)
	return RAISE_SYSERR(msp, sts);
    else
	return ML_CData (msp, addrBuf, addrLen);

} /* end of _ml_Sock_getsockname */
