/* ctlKEEPALIVE.c
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
#include "sock-util.h"
#include "cfun-proto-list.h"

/* _ml_Sock_ctlKEEPALIVE : (sock * bool option) -> bool
 */
ml_val_t _ml_Sock_ctlKEEPALIVE (ml_state_t *msp, ml_val_t arg)
{
    return _util_Sock_ControlFlg (msp, arg, SO_KEEPALIVE);

} /* end of _ml_Sock_ctlKEEPALIVE */
