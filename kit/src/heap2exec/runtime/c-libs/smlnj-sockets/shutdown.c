/* shutdown.c
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
#include "cfun-proto-list.h"

/* _ml_Sock_shutdown : (sock * int) -> unit
 */
ml_val_t _ml_Sock_shutdown (ml_state_t *msp, ml_val_t arg)
{
    if (shutdown (REC_SELINT(arg, 0), REC_SELINT(arg, 1)) < 0)
	return RAISE_SYSERR(msp, sts);
    else
	return ML_unit;

} /* end of _ml_Sock_shutdown */
