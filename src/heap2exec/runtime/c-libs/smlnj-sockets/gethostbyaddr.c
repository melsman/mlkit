/* gethostbyaddr.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "sockets-osdep.h"
#include INCLUDE_TYPES_H
#include INCLUDE_SOCKET_H
#include INCLUDE_IN_H
#include <netdb.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include "sock-util.h"

/* _ml_NetDB_gethostbyaddr
 *     : addr -> (string * string list * addr_family * addr list) option
 */
ml_val_t _ml_NetDB_gethostbyaddr (ml_state_t *msp, ml_val_t arg)
{
    ASSERT (sizeof(struct in_addr) == OBJ_LEN(arg));

    return _util_NetDB_mkhostent (
	msp,
	gethostbyaddr (PTR_MLtoC(char, arg), sizeof(struct in_addr), AF_INET));

} /* end of _ml_NetDB_gethostbyaddr */
