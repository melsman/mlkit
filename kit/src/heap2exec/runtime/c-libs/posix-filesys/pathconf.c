/* pathconf.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <unistd.h>
#include <errno.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include "../posix-error/posix-name-val.h"

 /* The following table is generated from all _PC_ values
  * in unistd.h. For most systems, this will include
    _PC_CHOWN_RESTRICTED
    _PC_LINK_MAX
    _PC_MAX_CANON
    _PC_MAX_INPUT
    _PC_NAME_MAX
    _PC_NO_TRUNC
    _PC_PATH_MAX
    _PC_PIPE_BUF
    _PC_VDISABLE
  *
  * The full POSIX list is given in section 5.7.1 of Std 1003.1b-1993.
  *
  * The SML string used to look up these values has the same
  * form but without the prefix, e.g., to lookup _PC_LINK_MAX,
  * use pathconf (path, "LINK_MAX")
  */
static name_val_t values[] = {
#include "ml_pathconf.h"
};

#define NUMELMS ((sizeof values)/(sizeof (name_val_t)))

/* mkValue
 *
 * Convert return value from (f)pathconf to ML value.
 */
static ml_val_t mkValue (ml_state_t *msp, int val)
{
    ml_val_t    p, obj;

    if (val >= 0) {
	WORD_ALLOC (msp, p, val);
	OPTION_SOME(msp, obj, p);
    }
    else if (errno == 0)
	obj = OPTION_NONE;
    else
	obj = RAISE_SYSERR(msp, val);

    return obj;

}  /* end of mkValue */

/* _ml_P_FileSys_pathconf : string * string -> word option
 *                          filename attribute
 *
 * Get configurable pathname attribute given pathname
 */
ml_val_t _ml_P_FileSys_pathconf (ml_state_t *msp, ml_val_t arg)
{
    int		val;
    char        *pathname = REC_SELPTR(char, arg, 0);
    name_val_t  *attr;

    attr = _ml_posix_nv_lookup (REC_SELPTR(char, arg, 1), values, NUMELMS);
    if (!attr) {
	errno = EINVAL;
	return RAISE_SYSERR(msp, -1);
    }
 
    errno = 0;
    while (((val = pathconf (pathname, attr->val)) == -1) && (errno == EINTR)) {
        errno = 0;
        continue;
    }

    return (mkValue (msp, val));

} /* end of _ml_P_FileSys_pathconf */

/* _ml_P_FileSys_fpathconf : int * string -> word option
 *                           fd     attribute
 *
 * Get configurable pathname attribute given pathname
 */
ml_val_t _ml_P_FileSys_fpathconf (ml_state_t *msp, ml_val_t arg)
{
    int		val;
    int         fd = REC_SELINT(arg, 0);
    name_val_t  *attr;

    attr = _ml_posix_nv_lookup (REC_SELPTR(char, arg, 1), values, NUMELMS);
    if (!attr) {
	errno = EINVAL;
	return RAISE_SYSERR(msp, -1);
    }
 
    errno = 0;
    while (((val = fpathconf (fd, attr->val)) == -1) && (errno == EINTR)) {
        errno = 0;
        continue;
    }

    return mkValue (msp, val);

} /* end of _ml_P_FileSys_fpathconf */
