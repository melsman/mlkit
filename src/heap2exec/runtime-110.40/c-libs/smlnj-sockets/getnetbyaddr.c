/* getnetbyaddr.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "sockets-osdep.h"
#include <netdb.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include "sock-util.h"

/* _ml_NetDB_getnetbyaddr
 *     : (sysword * addr_family) -> (string * string list * addr_family * sysword) option
 */
ml_val_t _ml_NetDB_getnetbyaddr (ml_state_t *msp, ml_val_t arg)
{
    unsigned long   net = REC_SELWORD(arg, 0);
    int		    type = REC_SELINT(arg, 1);

    return _util_NetDB_mknetent (msp, getnetbyaddr(net, type));

} /* end of _ml_NetDB_getnetbyaddr */
