/* setptimer.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * NOTE: this implementation is UNIX specific right now; I would like to
 * define an OS abstraction layer for interval timers, which would cover
 * both alarm timers and profiling, but I need to look at what other systems
 * do first.
 */

#ifdef OPSYS_UNIX
#  include "ml-unixdep.h"
#  include <sys/time.h>
#endif
#include "ml-base.h"
#include "ml-c.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"
#include "profile.h"

/* _ml_Prof_setptimer : bool -> unit
 *
 * Turn the profile timer on/off.
 */
ml_val_t _ml_Prof_setptimer (ml_state_t *msp, ml_val_t arg)
{
#ifdef HAS_SETITIMER
    struct itimerval	new_itv;
    int			sts;


    if (arg == ML_false) {
	new_itv.it_interval.tv_sec	=
	new_itv.it_value.tv_sec		=
	new_itv.it_interval.tv_usec	=
	new_itv.it_value.tv_usec	= 0;
    }
    else if (ProfCntArray == ML_unit) {
	return RAISE_ERROR(msp, "no count array set");
    }
    else {
	new_itv.it_interval.tv_sec	=
	new_itv.it_value.tv_sec		= 0;
	new_itv.it_interval.tv_usec	=
	new_itv.it_value.tv_usec	= PROFILE_QUANTUM_US;
    }

    sts = setitimer (ITIMER_VIRTUAL, &new_itv, NIL(struct itimerval *));

    CHK_RETURN_UNIT(msp, sts);

#else
    return RAISE_ERROR(msp, "time profiling not supported");
#endif

} /* end of _ml_Prof_setptimer */

