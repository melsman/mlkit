/* from-inetaddr.c
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 */

#include "ml-unixdep.h"
#include "sockets-osdep.h"
#include INCLUDE_TYPES_H
#include INCLUDE_SOCKET_H
#include INCLUDE_IN_H
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"


/* _ml_Sock_frominetaddr : addr -> (in_addr * int)
 *
 * Given a INET-domain socket address, return the INET address and port number.
 */
ml_val_t _ml_Sock_frominetaddr (ml_state_t *msp, ml_val_t arg)
{
    struct sockaddr_in	*addr = GET_SEQ_DATAPTR(struct sockaddr_in, arg);
    ml_val_t		data, inAddr, res;

    ASSERT (addr->sin_family == AF_INET);

    data = ML_CData (msp, &(addr->sin_addr), sizeof(struct in_addr));
    SEQHDR_ALLOC (msp, inAddr, DESC_word8vec, data, sizeof(struct in_addr));
    REC_ALLOC2 (msp, res, inAddr, INT_CtoML(ntohs(addr->sin_port)));

    return res;

} /* end of _ml_Sock_frominetaddr */

