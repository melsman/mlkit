/* util-mkhostent.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "sockets-osdep.h"
#include INCLUDE_TYPES_H
#include INCLUDE_SOCKET_H
#include <netdb.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "sock-util.h"

/* _util_NetDB_mkhostent:
 *
 * Allocate an ML value of type
 *    (string * string list * addr_family * addr list) option
 * to represent a struct hostent value.
 *
 * NOTE: we should probably be passing back the value of h_errno, but this
 * will require an API change at the SML level.
 */
ml_val_t _util_NetDB_mkhostent (ml_state_t *msp, struct hostent *hentry)
{
    if (hentry == NIL(struct hostent *))
	return OPTION_NONE;
    else {
      /* build the return result */
	ml_val_t	name, aliases, af, addr, addrs, res;
	int		nAddrs, i;

	name = ML_CString(msp, hentry->h_name);
	aliases = ML_CStringList(msp, hentry->h_aliases);
	af = ML_SysConst (msp, &_Sock_AddrFamily, hentry->h_addrtype);
	for (nAddrs = 0;  hentry->h_addr_list[nAddrs] != NIL(char *);  nAddrs++)
	    continue;
	for (i = nAddrs, addrs = LIST_nil;  --i >= 0;  ) {
	    addr = ML_CData(msp, hentry->h_addr_list[i], hentry->h_length);
	    LIST_cons(msp, addrs, addr, addrs);
	}
	REC_ALLOC4 (msp, res, name, aliases, af, addrs);
	OPTION_SOME (msp, res, res);
	return res;
    }

} /* end of _util_NetDB_mkhostent */
