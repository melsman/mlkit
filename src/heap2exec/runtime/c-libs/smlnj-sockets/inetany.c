/* inetany.c
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


/* _ml_Sock_inetany : int -> addr
 *
 * Make an INET_ANY INET socket address, with the given port ID.
 */
ml_val_t _ml_Sock_inetany (ml_state_t *msp, ml_val_t arg)
{
    struct sockaddr_in	addr;

    memset(&addr, 0, sizeof(struct sockaddr_in));

    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
    addr.sin_port = htons(INT_MLtoC(arg));

    return ML_CData (msp, &addr, sizeof(struct sockaddr_in));

} /* end of _ml_Sock_inetany */

