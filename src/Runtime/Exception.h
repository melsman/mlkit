/*----------------------------------------------------------------*
 *                      Exceptions                                *
 *----------------------------------------------------------------*/

#ifndef EXCEPTION
#define EXCEPTION

#include<signal.h>

#if defined(hpux)
typedef void (*SignalHandler)(__harg);  
#elif defined(sun)
typedef void (*SignalHandler)(int);  /* was void */
#elif defined(linux) || defined(_WIN32)
typedef void (*SignalHandler)(int);
#endif

extern int *exn_OVERFLOW;                 /* Exception raised for all primitive operations which
					     can generate an overflow. */

extern int *exn_INTERRUPT;                /* Exception for user interrupt (Ctrl-C). */

void raise_exn(int exn);

#endif /*EXCEPTION*/
