/* getprotbyname.c
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

/* _ml_NetDB_getprotbyname : string -> (string * string list * int) option
 */
ml_val_t _ml_NetDB_getprotbyname (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	    name, aliases, res;
    struct protoent *pentry;

    pentry = getprotobyname (PTR_MLtoC(char, arg));

    if (pentry == NIL(struct protoent *))
	return OPTION_NONE;
    else {
	name = ML_CString (msp, pentry->p_name);
	aliases = ML_CStringList (msp, pentry->p_aliases);
	REC_ALLOC3 (msp, res, name, aliases, INT_CtoML(pentry->p_proto));
	OPTION_SOME (msp, res, res);
	return res;
    }

} /* end of _ml_NetDB_getprotbyname */
