/* getcwd.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <unistd.h>
#include <limits.h>
#include <errno.h>
#include <sys/param.h>


/* _ml_P_FileSys_getcwd : unit -> string
 *
 * Get current working directory pathname
 *
 * Should this be written to avoid the extra copy?
 */
ml_val_t _ml_P_FileSys_getcwd (ml_state_t *msp, ml_val_t arg)
{
    char	path[MAXPATHLEN];
    char*	sts;
    ml_val_t    p;
    int         buflen;
    char        *buf;

    sts = getcwd(path, MAXPATHLEN);

    if (sts != NIL(char *))
	return ML_CString (msp, path);

    if (errno != ERANGE)
	return RAISE_SYSERR(msp, sts);

    buflen = 2*MAXPATHLEN;
    buf = MALLOC(buflen);
    if (buf == NIL(char*))
	return RAISE_ERROR(msp, "no malloc memory");

    while ((sts = getcwd(buf, buflen)) == NIL(char *)) {
        FREE (buf);
        if (errno != ERANGE)
	    return RAISE_SYSERR(msp, sts);
        else {
            buflen = 2*buflen;
            buf = MALLOC(buflen);
            if (buf == NIL(char*))
		return RAISE_ERROR(msp, "no malloc memory");
        }
    }
      
    p = ML_CString (msp, buf);
    FREE (buf);
      
    return p;

} /* end of _ml_P_FileSys_getcwd */
