/* util-mkservent.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "sockets-osdep.h"
#include INCLUDE_TYPES_H
#include INCLUDE_IN_H
#include <netdb.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "sock-util.h"

/* _util_NetDB_mkservent:
 *
 * Allocate an ML value of type:
 *    (string * string list * int * string) option
 * to represent a struct servent value.  Note that the port number is returned
 * in network byteorder, so we need to map it to host order.
 */
ml_val_t _util_NetDB_mkservent (ml_state_t *msp, struct servent *sentry)
{
    if (sentry == NIL(struct servent *))
	return OPTION_NONE;
    else {
      /* build the return result */
	ml_val_t	name, aliases, port, proto, res;

	name = ML_CString(msp, sentry->s_name);
	aliases = ML_CStringList(msp, sentry->s_aliases);
	port = INT_CtoML(ntohs(sentry->s_port));
	proto = ML_CString(msp, sentry->s_proto);
	REC_ALLOC4 (msp, res, name, aliases, port, proto);
	OPTION_SOME (msp, res, res);
	return res;
    }

} /* end of _util_NetDB_mkservent */
