/* ctlSNDBUF.c
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
#include "sock-util.h"

/* _ml_Sock_ctlSNDBUF : (sock * int option) -> int
 */
ml_val_t _ml_Sock_ctlSNDBUF (ml_state_t *msp, ml_val_t arg)
{
    int		sock = REC_SELINT(arg, 0);
    ml_val_t	ctl = REC_SEL(arg, 1);
    int		sz, sts;

    if (ctl == OPTION_NONE) {
	int	optSz = sizeof(int);
	sts = getsockopt (sock, SOL_SOCKET, SO_SNDBUF, (sockoptval_t)&sz, &optSz);
	ASSERT((sts < 0) || (optSz == sizeof(int)));
    }
    else {
	sz = INT_MLtoC(OPTION_get(ctl));
	sts = setsockopt (sock, SOL_SOCKET, SO_SNDBUF, (sockoptval_t)&sz, sizeof(int));
    }

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);
    else
	return INT_CtoML(sz);

} /* end of _ml_Sock_ctlSNDBUF */
