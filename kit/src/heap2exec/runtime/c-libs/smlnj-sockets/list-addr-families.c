/* list-addr-families.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-objects.h"
#include "sock-util.h"
#include "cfun-proto-list.h"

/* _ml_Sock_listaddrfamilies:
 *
 * Return a list of the known address famlies (this may contain unsupported
 * families).
 */
ml_val_t _ml_Sock_listaddrfamilies (ml_state_t *msp, ml_val_t arg)
{
    return ML_SysConstList (msp, &_Sock_AddrFamily);

} /* end of _ml_Sock_listaddrfamilies */
