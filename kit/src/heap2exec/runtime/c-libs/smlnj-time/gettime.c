/* gettime.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "vproc-state.h"
#include "ml-state.h"
#include "ml-timer.h"
#include "cfun-proto-list.h"

/* _ml_Time_gettime : unit -> (Int32.int * int * Int32.int * int * Int32.int * int)
 *
 * Return the total CPU time, system time and garbage collection time used by this
 * process so far.
 */
ml_val_t _ml_Time_gettime (ml_state_t *msp, ml_val_t arg)
{
    Time_t		t, s;
    ml_val_t		tSec, sSec, gcSec, res;
    vproc_state_t	*vsp = msp->ml_vproc;

    GetCPUTime (&t, &s);

    INT32_ALLOC (msp, tSec, t.seconds);
    INT32_ALLOC (msp, sSec, s.seconds);
    INT32_ALLOC (msp, gcSec, vsp->vp_gcTime->seconds);
    REC_ALLOC6 (msp, res,
	tSec, INT_CtoML(t.uSeconds),
	sSec, INT_CtoML(s.uSeconds),
	gcSec, INT_CtoML(vsp->vp_gcTime->uSeconds));

    return res;

} /* end of _ml_Time_gettime */

