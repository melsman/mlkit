/* recvbuf.c
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

/* _ml_Sock_recvbuf : (sock * Word8Array.array * int * int * bool * bool) -> int
 *
 * The arguments are: socket, data buffer, start position, number of
 * bytes, OOB flag and peek flag.
 */
ml_val_t _ml_Sock_recvbuf (ml_state_t *msp, ml_val_t arg)
{
    int		sock = REC_SELINT(arg, 0);
    ml_val_t	buf = REC_SEL(arg, 1);
    int		nbytes = REC_SELINT(arg, 3);
    char	*start = STR_MLtoC(buf) + REC_SELINT(arg, 2);
    int		flag = 0;
    int		n;

    if (REC_SEL(arg, 4) == ML_true) flag |= MSG_OOB;
    if (REC_SEL(arg, 5) == ML_true) flag |= MSG_PEEK;

    n = recv (sock, start, nbytes, flag);

    CHK_RETURN (msp, n)

} /* end of _ml_Sock_recvbuf */

