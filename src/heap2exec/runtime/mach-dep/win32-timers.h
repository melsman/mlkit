/* win32-timers.h
 *
 * COPYRIGHT (c) 1997 Bell Laboratories, Lucent Technologies
 *
 * header for
 * win32 specific interface to times and 
 * to an interface for interval timers.
 */

extern bool_t win32StopTimer();
extern bool_t win32StartTimer(int milli_secs);

/* end of win32-timers.h */
