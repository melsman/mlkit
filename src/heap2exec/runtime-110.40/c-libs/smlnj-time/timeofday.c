/* timeofday.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 */

#  include "ml-osdep.h"
#if defined(HAS_GETTIMEOFDAY)
#  if defined(OPSYS_WIN32)
#    include <sys/types.h>
#    include <sys/timeb.h>
#  else
#    include <sys/time.h>
#  endif
#else
#  error no timeofday mechanism
#endif   
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

/* _ml_Time_timeofday : unit -> (Int32.int * int)
 *
 * Return the time of day.
 * NOTE: gettimeofday() is not POSIX (time() returns seconds, and is POSIX
 * and ISO C).
 */
ml_val_t _ml_Time_timeofday (ml_state_t *msp, ml_val_t arg)
{
    int			c_sec, c_usec;
    ml_val_t		ml_sec, res;

#ifdef HAS_GETTIMEOFDAY
#if defined(OPSYS_UNIX)
    {
	struct timeval	t;

	gettimeofday (&t, NIL(struct timezone *));
	c_sec = t.tv_sec;
	c_usec = t.tv_usec;
    }
#elif defined(OPSYS_WIN32)
  /* we could use Win32 GetSystemTime/SystemTimetoFileTime here,
   * but the conversion routines for 64-bit 100-ns values
   * (in the mapi dll) are non-Win32s
   *
   * we'll use time routines from the C runtime for now.
   */
    {
	struct _timeb t;

	_ftime(&t);
	c_sec = t.time;
	c_usec = t.millitm*1000;
    }
#else
#error timeofday not defined for OS
#endif
#else
#error no timeofday mechanism
#endif

    INT32_ALLOC(msp, ml_sec, c_sec);
    REC_ALLOC2 (msp, res, ml_sec, INT_CtoML(c_usec));

    return res;

} /* end of _ml_Time_timeofday */

