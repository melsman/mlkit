/*----------------------------------------------------------------*
 *                      Exceptions                                *
 *----------------------------------------------------------------*/

#ifndef EXCEPTION
#define EXCEPTION

#include<signal.h>

#if defined(hpux)
typedef void (*SignalHandler)(__harg);  /* I don't know if __harg is architecture dependent...  18/05/1996-Martin */ 
#elif defined(sun)
typedef void (*SignalHandler)(void);
#endif

extern int *Overflow_val;                 /* Exception raised for all primitive operations which
					     can generate an overflow. */

extern int *Interrupt_val;                /* Exception for user interrupt (Ctrl-C). */

void raise_exn(int exn);

#endif /*EXCEPTION*/
