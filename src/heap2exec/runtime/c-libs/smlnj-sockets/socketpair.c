/* socketpair.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * NOTE: this file is UNIX specific.
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

/* _ml_Sock_socketpair : (int * int * int) -> (sock * sock)
 *
 * Create a pair of sockets.  The arguments are: domain (should be
 * AF_UNIX), type, and protocol.
 */
ml_val_t _ml_Sock_socketpair (ml_state_t *msp, ml_val_t arg)
{
    int		domain = REC_SELINT(arg, 0);
    int		type = REC_SELINT(arg, 1);
    int		protocol = REC_SELINT(arg, 2);
    int		sts, sock[2];

    sts = socketpair (domain, type, protocol, sock);

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);
    else {
	ml_val_t	res;
	REC_ALLOC2(msp, res, INT_CtoML(sock[0]), INT_CtoML(sock[1]));
	return res;
    }

} /* end of _ml_Sock_socketpair */
