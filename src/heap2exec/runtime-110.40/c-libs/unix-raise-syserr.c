/* unix-raise-syserr.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#ifdef HAS_STRERROR
#  include <string.h>
#endif
#include <stdio.h>
#include <errno.h>
#include "ml-base.h"
#include "ml-state.h"
#include "ml-objects.h"
#include "ml-globals.h"
#include "ml-c.h"


#ifndef HAS_STRERROR
/* strerror:
 * An implementation of strerror for those systems that do not provide it.
 */
PVT char *strerror (int errnum)
{
    extern int	sys_nerr;
    extern char	*sys_errlist[];

    if ((errnum < 0) || (sys_nerr <= errnum))
	return "<unknown system error>";
    else
	return sys_errlist[errnum];

} /* end of strerror */
#endif


/* RaiseSysError:
 *
 * Raise the ML exception SysErr, which has the spec:
 *
 *    exception SysErr of (string * syserror option)
 *
 * For the time being, we use the errno value as the syserror; eventually that
 * will be represented by an (int * string) pair.  If alt_msg is non-zero,
 * then use it as the error string and use NONE for the syserror.
 */
ml_val_t RaiseSysError (ml_state_t *msp, const char *altMsg, const char *at)
{
    ml_val_t	    s, atStk, syserror, arg, exn;
    const char	    *msg;
    char	    buf[32];

    if (altMsg != NIL(char *)) {
	msg = altMsg;
	syserror = OPTION_NONE;
    }
    else if ((msg = strerror(errno)) != NIL(char *)) {
	OPTION_SOME(msp, syserror, INT_CtoML(errno))
    }
    else {
	sprintf(buf, "<unknown error %d>", errno);
	msg = buf;
	OPTION_SOME(msp, syserror, INT_CtoML(errno));
    }

#if (defined(DEBUG_OS_INTERFACE) || defined(DEBUG_TRACE_CCALL))
    SayDebug ("RaiseSysError: errno = %d, msg = \"%s\"\n",
	(altMsg != NIL(char *)) ? -1 : errno, msg);
#endif

    s = ML_CString (msp, msg);
    if (at != NIL(char *)) {
	ml_val_t atMsg = ML_CString (msp, at);
	LIST_cons(msp, atStk, atMsg, LIST_nil);
    }
    else
	atStk = LIST_nil;
    REC_ALLOC2 (msp, arg, s, syserror);
    EXN_ALLOC (msp, exn, PTR_CtoML(SysErrId), arg, atStk);

    RaiseMLExn (msp, exn);

    return exn;

} /* end of RaiseSysError */
