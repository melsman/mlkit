/* to-unixaddr.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "sockets-osdep.h"
#include INCLUDE_TYPES_H
#include INCLUDE_SOCKET_H
#include INCLUDE_UN_H
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"


/* _ml_Sock_tounixaddr : string -> addr
 *
 * Given a path, allocate a UNIX-domain socket address.
 */
ml_val_t _ml_Sock_tounixaddr (ml_state_t *msp, ml_val_t arg)
{
    char		*path = STR_MLtoC(arg);
    struct sockaddr_un	addr;
    int			len;
    ml_val_t		data, res;

    memset(&addr, 0, sizeof(struct sockaddr_un));

    addr.sun_family = AF_UNIX;
    strcpy (addr.sun_path, path);
#ifdef SOCKADDR_HAS_LEN
    len = strlen(path)+sizeof(addr.sun_len)+sizeof(addr.sun_family)+1;
    addr.sun_len = len;
#else
    len = strlen(path)+sizeof(addr.sun_family)+1;
#endif

    data = ML_CData (msp, &addr, len);
    SEQHDR_ALLOC (msp, res, DESC_word8vec, data, len);

    return res;

} /* end of _ml_Sock_tounixaddr */

