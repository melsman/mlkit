/* connect.c
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

/* _ml_Sock_connect : (sock * addr) -> unit
 */
ml_val_t _ml_Sock_connect (ml_state_t *msp, ml_val_t arg)
{
    int		sock = REC_SELINT(arg, 0);
    ml_val_t	addr = REC_SEL(arg, 1);
    int		sts;

    sts = connect (sock, PTR_MLtoC(struct sockaddr, addr), OBJ_LEN(addr));

    CHK_RETURN_UNIT(msp, sts);

} /* end of _ml_Sock_connect */
