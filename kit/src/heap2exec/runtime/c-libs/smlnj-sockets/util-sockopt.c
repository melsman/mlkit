/* util-sockopt.c
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

/* _util_Sock_ControlFlg:
 *
 * This utility routine gets/sets a boolean socket option.
 */
ml_val_t _util_Sock_ControlFlg (ml_state_t *msp, ml_val_t arg, int option)
{
    int		sock = REC_SELINT(arg, 0);
    ml_val_t	ctl = REC_SEL(arg, 1);
    int		flg, sts;

    if (ctl == OPTION_NONE) {
	int	optSz = sizeof(int);
	sts = getsockopt (sock, SOL_SOCKET, option, (sockoptval_t)&flg, &optSz);
	ASSERT((sts < 0) || (optSz == sizeof(int)));
    }
    else {
	flg = INT_MLtoC(OPTION_get(ctl));
	sts = setsockopt (sock, SOL_SOCKET, option, (sockoptval_t)&flg, sizeof(int));
    }

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);
    else
	return (flg ? ML_true : ML_false);

} /* end of _util_Sock_ControlFlg */

