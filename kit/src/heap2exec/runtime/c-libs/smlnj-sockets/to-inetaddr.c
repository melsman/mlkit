/* to-inetaddr.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
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


/* _ml_Sock_toinetaddr : (in_addr * int) -> addr
 *
 * Given a INET address and port number, allocate a INET-domain socket address.
 */
ml_val_t _ml_Sock_toinetaddr (ml_state_t *msp, ml_val_t arg)
{
    struct sockaddr_in	addr;

    memset(&addr, 0, sizeof(struct sockaddr_in));

    addr.sin_family = AF_INET;
    memcpy (&addr.sin_addr, REC_SELPTR(char, arg, 0), sizeof(struct in_addr));
    addr.sin_port = htons(REC_SELINT(arg, 1));

    return ML_CData (msp, &addr, sizeof(struct sockaddr_in));

} /* end of _ml_Sock_toinetaddr */

