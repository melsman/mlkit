/* sendbufto.c
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

/* _ml_Sock_sendbufto : (sock * bytes * int * int * bool * bool * addr) -> int
 *
 * Send data from the buffer; bytes is either a Word8Array.array, or
 * a Word8Vector.vector.  The arguments are: socket, data buffer, start
 * position, number of bytes, OOB flag, don't_route flag, and destination address.
 */
ml_val_t _ml_Sock_sendbufto (ml_state_t *msp, ml_val_t arg)
{
    int		sock = REC_SELINT(arg, 0);
    ml_val_t	buf = REC_SEL(arg, 1);
    int		nbytes = REC_SELINT(arg, 3);
    char	*data = STR_MLtoC(buf) + REC_SELINT(arg, 2);
    ml_val_t	addr = REC_SEL(arg, 6);
    int		flgs, n;

  /* initialize the flags. */
    flgs = 0;
    if (REC_SEL(arg, 4) == ML_true) flgs |= MSG_OOB;
    if (REC_SEL(arg, 5) == ML_true) flgs |= MSG_DONTROUTE;

    n = sendto (
	sock, data, nbytes, flgs,
	GET_SEQ_DATAPTR(struct sockaddr, addr), GET_SEQ_LEN(addr));

    CHK_RETURN (msp, n);

} /* end of _ml_Sock_sendbufto */
