/* readlink.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <unistd.h>
#include <sys/stat.h>
#include <limits.h>
#include <sys/param.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"


/* _ml_P_FileSys_readlink : string -> string
 *
 * Read the value of a symbolic link.
 *
 * The following implementation assumes that the system readlink
 * fills the given buffer as much as possible, without nul-termination,
 * and returns the number of bytes copied. If the buffer is not large
 * enough, the return value will be at least the buffer size. In that
 * case, we find out how big the link really is, allocate a buffer to
 * hold it, and redo the readlink.
 *
 * Note that the above semantics are not those of POSIX, which requires
 * null-termination on success, and only fills the buffer up to as most 
 * the penultimate byte even on failure.
 *
 * Should this be written to avoid the extra copy, using heap memory?
 */
ml_val_t _ml_P_FileSys_readlink (ml_state_t *msp, ml_val_t arg)
{
    char        *path = STR_MLtoC(arg);
    char	buf[MAXPATHLEN];
    int         len;

    len = readlink(path, buf, MAXPATHLEN);

    if (len < 0)
	return RAISE_SYSERR(msp, len);
    else if (len < MAXPATHLEN) {
	buf[len] = '\0';
	return ML_CString (msp, buf);
    }
    else {  /* buffer not big enough */
	char         *nbuf;
	ml_val_t     obj;
	struct stat  sbuf;
	int          res;
	int          nlen;

      /* Determine how big the link text is and allocate a buffer */
	res = lstat (path, &sbuf);
	if (res < 0)
	    return RAISE_SYSERR(msp, res);
	nlen = sbuf.st_size + 1;
	nbuf = MALLOC(nlen);
	if (nbuf == 0)
	    return RAISE_ERROR(msp, "out of malloc memory");

        /* Try the readlink again. Give up on error or if len is still bigger
         * than the buffer size.
         */
	len = readlink(path, buf, len);
	if (len < 0)
	    return RAISE_SYSERR(msp, len);
	else if (len >= nlen)
	    return RAISE_ERROR(msp, "readlink failure");

	nbuf[len] = '\0';
	obj = ML_CString (msp, nbuf);
	FREE (nbuf);
	return obj;
    }

} /* end of _ml_P_FileSys_readlink */
