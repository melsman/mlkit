/* getservbyport.c
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

/* _ml_NetDB_getservbyport
 *     : (int * string option) -> (string * string list * int * string) option
 */
ml_val_t _ml_NetDB_getservbyport (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	mlProto = REC_SEL(arg, 1);
    char	*proto;

    if (mlProto == OPTION_NONE)
	proto = NIL(char *);
    else
	proto = STR_MLtoC(OPTION_get(mlProto));

    return _util_NetDB_mkservent (msp, getservbyport (REC_SELINT(arg, 0), proto));

} /* end of _ml_NetDB_getservbyport */
