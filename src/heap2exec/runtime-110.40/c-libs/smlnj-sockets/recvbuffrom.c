/* recvbuffrom.c
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

/* _ml_Sock_recvbuffrom
 *   : (sock * Word8Array.array * int * int * bool * bool) -> (int * addr)
 *
 * The arguments are: socket, data buffer, start position, number of
 * bytes, OOB flag and peek flag.  The result is number of bytes read and
 * the source address.
 */
ml_val_t _ml_Sock_recvbuffrom (ml_state_t *msp, ml_val_t arg)
{
    char	addrBuf[MAX_SOCK_ADDR_SZB];
    int		addrLen = MAX_SOCK_ADDR_SZB;
    int		sock = REC_SELINT(arg, 0);
    ml_val_t	buf = REC_SEL(arg, 1);
    int		nbytes = REC_SELINT(arg, 3);
    char	*start = STR_MLtoC(buf) + REC_SELINT(arg, 2);
    int		flag = 0;
    int		n;

    if (REC_SEL(arg, 4) == ML_true) flag |= MSG_OOB;
    if (REC_SEL(arg, 5) == ML_true) flag |= MSG_PEEK;

    n = recvfrom (sock, start, nbytes, flag, (struct sockaddr *)addrBuf, &addrLen);

    if (n < 0)
	return RAISE_SYSERR(msp, sts);
    else {
	ml_val_t	data = ML_CData (msp, addrBuf, addrLen);
	ml_val_t	addr, res;

	SEQHDR_ALLOC (msp, addr, DESC_word8vec, data, addrLen);
	REC_ALLOC2(msp, res, INT_CtoML(n), addr);
	return res;
    }

} /* end of _ml_Sock_recvbuffrom */

