/* accept.c
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

/* _ml_Sock_accept : sock -> (sock * addr)
 */
ml_val_t _ml_Sock_accept (ml_state_t *msp, ml_val_t arg)
{
    int		sock = INT_MLtoC(arg);
    char	addrBuf[MAX_SOCK_ADDR_SZB];
    int		addrLen = MAX_SOCK_ADDR_SZB;
    int		newSock;

    newSock = accept (sock, (struct sockaddr *)addrBuf, &addrLen);

    if (newSock == -1)
	return RAISE_SYSERR(msp, newSock);
    else {
	ml_val_t	data = ML_CData (msp, addrBuf, addrLen);
	ml_val_t	addr, res;

	SEQHDR_ALLOC(msp, addr, DESC_word8vec, data, addrLen);
	REC_ALLOC2(msp, res, INT_CtoML(newSock), addr);
	return res;
    }

} /* end of _ml_Sock_accept */
