/* ctlNODELAY.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "sockets-osdep.h"
#include INCLUDE_TYPES_H
#include INCLUDE_SOCKET_H
#include INCLUDE_IN_H
#include INCLUDE_TCP_H
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include "sock-util.h"

/* _ml_Sock_ctlNODELAY : (sock * bool option) -> bool
 *
 * NOTE: this is a TCP level option, so we cannot use the utility function.
 */
ml_val_t _ml_Sock_ctlNODELAY (ml_state_t *msp, ml_val_t arg)
{
    int		sock = REC_SELINT(arg, 0);
    ml_val_t	ctl = REC_SEL(arg, 1);
    bool_t	flg;
    int		sts;

    if (ctl == OPTION_NONE) {
	int	optSz = sizeof(int);
	sts = getsockopt (sock, IPPROTO_TCP, TCP_NODELAY, (sockoptval_t)&flg, &optSz);
	ASSERT((sts < 0) || (optSz == sizeof(int)));
    }
    else {
	flg = (bool_t)INT_MLtoC(OPTION_get(ctl));
	sts = setsockopt (sock, IPPROTO_TCP, TCP_NODELAY, (sockoptval_t)&flg, sizeof(int));
    }

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);
    else
	return (flg ? ML_true : ML_false);

} /* end of _ml_Sock_ctlNODELAY */

