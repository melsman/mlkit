/* uname.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <sys/utsname.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_ProcEnv_uname: unit -> (string * string) list
 *
 * Return names of current system.
 */
ml_val_t _ml_P_ProcEnv_uname (ml_state_t *msp, ml_val_t arg)
{
    struct utsname      name;
    int                 sts;
    ml_val_t            l, p, s;
    ml_val_t		field;

    sts = uname (&name);

    if (sts == -1)
	RAISE_SYSERR(msp, sts);

/** NOTE: we should do something about possible GC!!! **/

    l = LIST_nil;

    field = ML_CString(msp, "machine");
    s = ML_CString(msp, name.machine);
    REC_ALLOC2(msp, p, field, s);
    LIST_cons(msp, l, p, l);

    field = ML_CString(msp, "version");
    s = ML_CString(msp, name.version);
    REC_ALLOC2(msp, p, field, s);
    LIST_cons(msp, l, p, l);

    field = ML_CString(msp, "release");
    s = ML_CString(msp, name.release);
    REC_ALLOC2(msp, p, field, s);
    LIST_cons(msp, l, p, l);

    field = ML_CString(msp, "nodename");
    s = ML_CString(msp, name.nodename);
    REC_ALLOC2(msp, p, field, s);
    LIST_cons(msp, l, p, l);

    field = ML_CString(msp, "sysname");
    s = ML_CString(msp, name.sysname);
    REC_ALLOC2(msp, p, field, s);
    LIST_cons(msp, l, p, l);

    return l;

} /* end of _ml_P_ProcEnv_uname */

