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

/* _ml_Sock_recvfrom : (sock * int * bool * bool) -> (Word8Vector.vector * addr)
 *
 * The arguments are: socket, number of bytes, OOB flag and peek flag.  The
 * result is the vector of bytes read and the source address.
 */
ml_val_t _ml_Sock_recvfrom (ml_state_t *msp, ml_val_t arg)
{
    char	addrBuf[MAX_SOCK_ADDR_SZB];
    int		addrLen = MAX_SOCK_ADDR_SZB;
    int		sock = REC_SELINT(arg, 0);
    int		nbytes = REC_SELINT(arg, 1);
    int		flag = 0;
    ml_val_t	vec;
    int		n;

    if (REC_SEL(arg, 2) == ML_true) flag |= MSG_OOB;
    if (REC_SEL(arg, 3) == ML_true) flag |= MSG_PEEK;

  /* allocate the vector; note that this might cause a GC */
    vec = ML_AllocRaw32 (msp, BYTES_TO_WORDS(nbytes));

    n = recvfrom (
	sock, PTR_MLtoC(char, vec), nbytes, flag,
	(struct sockaddr *)addrBuf, &addrLen);

    if (n < 0)
	return RAISE_SYSERR(msp, sts);
    else {
	ml_val_t	data = ML_CData (msp, addrBuf, addrLen);
	ml_val_t	addr, res;

	if (n == 0)
	    res = ML_string0;
	else {
	    if (n < nbytes)
	      /* we need to shrink the vector */
		ML_ShrinkRaw32 (msp, vec, BYTES_TO_WORDS(n));
	    SEQHDR_ALLOC (msp, res, DESC_string, vec, n);
	}

	SEQHDR_ALLOC (msp, addr, DESC_word8vec, data, addrLen);
	REC_ALLOC2(msp, res, res, addr);

	return res;
    }

} /* end of _ml_Sock_recvfrom */

