/* ctlLINGER.c
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
#include "sock-util.h"
#include "cfun-proto-list.h"

/* _ml_Sock_ctlLINGER : (sock * int option option) -> int option
 *
 * Set/get the SO_LINGER option as follows:
 *   NONE		=> get current setting
 *   SOME(NONE)		=> disable linger
 *   SOME(SOME t)	=> enable linger with timeout t.
 */
ml_val_t _ml_Sock_ctlLINGER (ml_state_t *msp, ml_val_t arg)
{
    int		    sock = REC_SELINT(arg, 0);
    ml_val_t	    ctl = REC_SEL(arg, 1);
    struct linger   optVal;
    int		    sts;

    if (ctl == OPTION_NONE) {
	int	optSz = sizeof(struct linger);
	sts = getsockopt (sock, SOL_SOCKET, SO_LINGER, (sockoptval_t)&optVal, &optSz);
	ASSERT((sts < 0) || (optSz == sizeof(struct linger)));
    }
    else {
	ctl = OPTION_get(ctl);
	if (ctl == OPTION_NONE) {
	  /* argument is SOME(NONE); disable linger */
	    optVal.l_onoff = 0;
	}
	else {
	  /* argument is SOME t; enable linger */
	    optVal.l_onoff = 1;
	    optVal.l_linger = INT_MLtoC(OPTION_get(ctl));
	}
	sts = setsockopt (sock, SOL_SOCKET, SO_LINGER, (sockoptval_t)&optVal, sizeof(struct linger));
    }

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);
    else if (optVal.l_onoff == 0)
	return OPTION_NONE;
    else {
	ml_val_t	res;
	OPTION_SOME(msp, res, INT_CtoML(optVal.l_linger));
	return res;
    }

} /* end of _ml_Sock_ctlLINGER */
