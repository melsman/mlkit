/* ml-timer.h
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 */

#ifndef _ML_TIMER_
#define _ML_TIMER_

#ifndef _ML_BASE_
#include "ml-base.h"
#endif

/* we define our own type to represent time values, since some systems have
 * struct timeval, but others do not.
 */
typedef struct {
    Int32_t	seconds;
    Int32_t	uSeconds;
} Time_t;

extern void GetCPUTime (Time_t *user_t, Time_t *sys_t);
extern void StartGCTimer (vproc_state_t *vsp);
extern void StopGCTimer (vproc_state_t *vsp, long *time);

#endif /* !_ML_TIMER_ */
