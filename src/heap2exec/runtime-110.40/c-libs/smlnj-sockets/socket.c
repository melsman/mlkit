/* socket.c
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

/* _ml_Sock_socket : (int * int * int) -> sock
 */
ml_val_t _ml_Sock_socket (ml_state_t *msp, ml_val_t arg)
{
    int		domain = REC_SELINT(arg, 0);
    int		type = REC_SELINT(arg, 1);
    int		protocol = REC_SELINT(arg, 2);
    int		sock;

    sock = socket (domain, type, protocol);
    if (sock < 0)
	return RAISE_SYSERR(msp, sts);
    else
	return INT_CtoML(sock);

} /* end of _ml_Sock_socket */
