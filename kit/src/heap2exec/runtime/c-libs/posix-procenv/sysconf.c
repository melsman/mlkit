/* sysconf.c
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

 /* The following table is generated from all _SC_ values
  * in unistd.h. For most systems, this will include
  * _SC_ARG_MAX
  * _SC_CHILD_MAX
  * _SC_CLK_TCK
  * _SC_JOB_CONTROL
  * _SC_NGROUPS_MAX
  * _SC_OPEN_MAX
  * _SC_SAVED_IDS
  * _SC_STREAM_MAX
  * _SC_TZNAME_MAX
  * _SC_VERSION
  *
  * The full POSIX list is given in section 4.8.1 of Std 1003.1b-1993.
  *
  * The SML string used to look up these values has the same
  * form but without the prefix, e.g., to lookup _SC_ARG_MAX,
  * use sysconf "ARG_MAX"
  */
static name_val_t values[] = {
#include "ml_sysconf.h"
};

#define NUMELMS ((sizeof values)/(sizeof (name_val_t)))

/* _ml_P_ProcEnv_sysconf : string -> word
 *
 *
 * Get configurable system variables
 */
ml_val_t _ml_P_ProcEnv_sysconf (ml_state_t *msp, ml_val_t arg)
{
    long	val;
    name_val_t  *attr;
    ml_val_t	p;

    attr = _ml_posix_nv_lookup (PTR_MLtoC(char, arg), values, NUMELMS);
    if (!attr) {
      errno = EINVAL;
      return RAISE_SYSERR(msp, -1);
    }
 
    errno = 0;
    while (((val = sysconf(attr->val)) == -1) && (errno == EINTR)) {
      errno = 0;
      continue;
    }

    if (val >= 0) {
      WORD_ALLOC (msp, p, val);
      return p;
    }
    else if (errno == 0)
      return RAISE_ERROR(msp, "unsupported POSIX feature");
    else
      return RAISE_SYSERR(msp, -1);

} /* end of _ml_P_ProcEnv_sysconf */
