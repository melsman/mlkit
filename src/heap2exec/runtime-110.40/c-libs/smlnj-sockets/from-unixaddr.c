/* from-unixaddr.c
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 */

#include "ml-unixdep.h"
#include "sockets-osdep.h"
#include INCLUDE_TYPES_H
#include INCLUDE_SOCKET_H
#include INCLUDE_UN_H
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"


/* _ml_Sock_fromunixaddr : addr -> string
 *
 * Given a UNIX-domain socket address, return the string.
 */
ml_val_t _ml_Sock_fromunixaddr (ml_state_t *msp, ml_val_t arg)
{
    struct sockaddr_un	*addr = GET_SEQ_DATAPTR(struct sockaddr_un, arg);

    ASSERT(addr->sun_family == AF_UNIX);

    return ML_CString(msp, addr->sun_path);

} /* end of _ml_Sock_fromunixaddr */

