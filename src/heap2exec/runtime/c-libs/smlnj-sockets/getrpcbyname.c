/* getrpcbyname.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "sockets-osdep.h"
#include <netdb.h>
#ifdef INCLUDE_RPCENT_H
#  include INCLUDE_RPCENT_H
#endif
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include "sock-util.h"

/* _ml_NetDB_getrpcbyname : string -> (string * string list * int) option
 */
ml_val_t _ml_NetDB_getrpcbyname (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	    name, aliases, res;
    struct rpcent   *rentry;

    rentry = getrpcbyname (PTR_MLtoC(char, arg));

    if (rentry == NIL(struct rpcent *))
	return OPTION_NONE;
    else {
	name = ML_CString (msp, rentry->r_name);
	aliases = ML_CStringList (msp, rentry->r_aliases);
	REC_ALLOC3 (msp, res, name, aliases, INT_CtoML(rentry->r_number));
	OPTION_SOME (msp, res, res);
	return res;
    }

} /* end of _ml_NetDB_getrpcbyname */
