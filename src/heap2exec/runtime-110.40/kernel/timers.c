/* timers.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 *
 * OS independent timer routines; these rely on a OS dependent implementation
 * of the following function:
 *
 *	void GetCPUTime (Time_t *user_t, Time_t *sys_t);
 */

#include "ml-base.h"
#include "vproc-state.h"
#include "ml-timer.h"


/* ResetTimers:
 *
 * Clear the GC timers.
 */
void ResetTimers (vproc_state_t *vsp)
{
    vsp->vp_gcTime->seconds = 0;
    vsp->vp_gcTime->uSeconds = 0;

} /* end of ResetTimers. */


/* StartGCTimer:
 */
void StartGCTimer (vproc_state_t *vsp)
{
    GetCPUTime (vsp->vp_gcTime0, NIL(Time_t *));

} /* end of StartGCTimer */


/* StopGCTimer:
 *
 * Stop the garbage collection timer and update the cumulative garbage collection
 * time.  If time is not NIL, then return the time (in ms.) spent since
 * the start of the GC.
 */
void StopGCTimer (vproc_state_t *vsp, long *time)
{
    int			sec, usec;
    Time_t		t1;
    Time_t		*gt0 = vsp->vp_gcTime0;
    Time_t		*gt = vsp->vp_gcTime;

    GetCPUTime (&t1, NIL(Time_t *));

    sec = t1.seconds - gt0->seconds;
    usec = t1.uSeconds - gt0->uSeconds;

    if (time != NIL(long *)) {
	if (usec < 0) {
	    sec--; usec += 1000000;
	}
	else if (usec > 1000000) {
	    sec++; usec -= 1000000;
	}
	*time = (usec/1000 + sec*1000);
    }

    sec = gt->seconds + sec;
    usec = gt->uSeconds + usec;
    if (usec < 0) {
	sec--; usec += 1000000;
    }
    else if (usec > 1000000) {
	sec++; usec -= 1000000;
    }
    gt->seconds = sec;
    gt->uSeconds = usec;

} /* end of StopGCTimer */

