/* util-mknetent.c
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
#include "sock-util.h"

/* _util_NetDB_mknetent:
 *
 * Allocate an ML value of type
 *    (string * string list * addr_family * sysword) option
 * to represent a struct netent value.
 */
ml_val_t _util_NetDB_mknetent (ml_state_t *msp, struct netent *nentry)
{
    if (nentry == NIL(struct netent *))
	return OPTION_NONE;
    else {
      /* build the return result */
	ml_val_t	name, aliases, af, net, res;

	name = ML_CString(msp, nentry->n_name);
	aliases = ML_CStringList(msp, nentry->n_aliases);
	af = ML_SysConst (msp, &_Sock_AddrFamily, nentry->n_addrtype);
	WORD_ALLOC(msp, net, (Word_t)(nentry->n_net));
	REC_ALLOC4 (msp, res, name, aliases, af, net);
	OPTION_SOME (msp, res, res);
	return res;
    }

} /* end of _util_NetDB_mknetent */
