/* getnetbyname.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "sockets-osdep.h"
#include <netdb.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include "sock-util.h"

/* _ml_NetDB_getnetbyname : string -> (string * string list * addr_family * sysword) option
 */
ml_val_t _ml_NetDB_getnetbyname (ml_state_t *msp, ml_val_t arg)
{
    return _util_NetDB_mknetent (msp, getnetbyname (STR_MLtoC(arg)));

} /* end of _ml_NetDB_getnetbyname */
