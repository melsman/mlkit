/* system-signals.h
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 *
 * NOTE: this file is generated --- do not edit!!!
 */

#ifndef _SYSTEM_SIGNALS_
#define _SYSTEM_SIGNALS_

#define NUM_SYSTEM_SIGS 17
#define MAX_SYSTEM_SIG  28 /* SIGVTALRM */
#define NUM_SIGS        18
#define MAX_SIG         29 /* RUNSIG_GC */

#define RUNSIG_GC 29

#define IS_SYSTEM_SIG(S) ((S) <= MAX_SYSTEM_SIG)

#endif /* !_SYSTEM_SIGNALS_ */
