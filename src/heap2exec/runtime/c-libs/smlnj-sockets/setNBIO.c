/* setNBIO.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "sockets-osdep.h"
#include INCLUDE_TYPES_H
#include INCLUDE_SOCKET_H
#include <sys/ioctl.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include "sock-util.h"

/* _ml_Sock_setNBIO : (sock * bool) -> unit
 */
ml_val_t _ml_Sock_setNBIO (ml_state_t *msp, ml_val_t arg)
{
    int		n, sts;
    int		sock = REC_SELINT(arg, 0);

#ifdef USE_FCNTL_FOR_NBIO
    n = fcntl(F_GETFL, sock);
    if (n < 0)
	return RAISE_SYSERR (msp, n);
    if (REC_SEL(arg, 1) == ML_true)
	n |= O_NONBLOCK;
    else
	n &= ~O_NONBLOCK;
    sts = fcntl(F_SETFL, sock, n);
#else
    n = (REC_SEL(arg, 1) == ML_true);
    sts = ioctl (sock, FIONBIO, (char *)&n);
#endif

    CHK_RETURN_UNIT(msp, sts);

} /* end of _ml_Sock_setNBIO */
