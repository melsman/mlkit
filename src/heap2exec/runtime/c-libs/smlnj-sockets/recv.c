/* recv.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "sockets-osdep.h"
#include INCLUDE_TYPES_H
#include INCLUDE_SOCKET_H
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_Sock_recv : (sock * int * bool * bool) -> int
 *
 * The arguments are: socket, number of bytes, OOB flag and peek flag; the
 * result is the vector of bytes received.
 */
ml_val_t _ml_Sock_recv (ml_state_t *msp, ml_val_t arg)
{
    int		sock = REC_SELINT(arg, 0);
    int		nbytes = REC_SELINT(arg, 1);
    int		flag = 0;
    ml_val_t	vec;
    int		n;

    if (REC_SEL(arg, 2) == ML_true) flag |= MSG_OOB;
    if (REC_SEL(arg, 3) == ML_true) flag |= MSG_PEEK;

  /* allocate the vector; note that this might cause a GC */
    vec = ML_AllocString (msp, nbytes);

    n = recv (sock, PTR_MLtoC(char, vec), nbytes, flag);

    if (n < 0)
	return RAISE_SYSERR(msp, sts);
    else if (n < nbytes) {
      /* we need to correct the length in the descriptor */
	PTR_MLtoC(ml_val_t, vec)[-1] = MAKE_DESC(n, DTAG_string);
    }

    return vec;

} /* end of _ml_Sock_recv */

