/* getpeername.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "sockets-osdep.h"
#include INCLUDE_TYPES_H
#include INCLUDE_SOCKET_H
#include INCLUDE_IN_H  /* for htons */
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include "sock-util.h"

/* _ml_Sock_getpeername : sock -> (af * addr)
 */
ml_val_t _ml_Sock_getpeername (ml_state_t *msp, ml_val_t arg)
{
    char	    data[MAX_SOCK_ADDR_SZB];
    struct sockaddr *addr;
    int		    addrLen;

    addr = (struct sockaddr *)data;
    addrLen = MAX_SOCK_ADDR_SZB;
    if (getpeername (INT_MLtoC(arg), addr, &addrLen) < 0)
	return RAISE_SYSERR(msp, sts);
    else {
	ml_val_t	af = ML_SysConst (msp, &_Sock_AddrFamily,
				ntohs(addr->sa_family));
	ml_val_t	cdata = ML_CData(msp, addr, addrLen);
	ml_val_t	res;

	REC_ALLOC2 (msp, res, af, cdata);
	return res;
    }

} /* end of _ml_Sock_getpeername */
