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

PVT ML_STRING(sysname,"sysname");
PVT ML_STRING(nodename,"nodename");
PVT ML_STRING(release,"release");
PVT ML_STRING(version,"version");
PVT ML_STRING(machine,"machine");

/* _ml_P_ProcEnv_uname: unit -> (string * string) list
 *
 * Return names of current system.
 */
ml_val_t _ml_P_ProcEnv_uname (ml_state_t *msp, ml_val_t arg)
{
    struct utsname      name;
    int                 sts;
    ml_val_t            l, p, s;

    sts = uname (&name);

    if (sts == -1)
	RAISE_SYSERR(msp, sts);

/** NOTE: we should do something about possible GC!!! **/

    l = LIST_nil;
    s = ML_CString(msp, name.machine);
    REC_ALLOC2(msp, p, PTR_CtoML(machine.s), s);
    LIST_cons(msp, l, p, l);
    s = ML_CString(msp, name.version);
    REC_ALLOC2(msp, p, PTR_CtoML(version.s), s);
    LIST_cons(msp, l, p, l);
    s = ML_CString(msp, name.release);
    REC_ALLOC2(msp, p, PTR_CtoML(release.s), s);
    LIST_cons(msp, l, p, l);
    s = ML_CString(msp, name.nodename);
    REC_ALLOC2(msp, p, PTR_CtoML(nodename.s), s);
    LIST_cons(msp, l, p, l);
    s = ML_CString(msp, name.sysname);
    REC_ALLOC2(msp, p, PTR_CtoML(sysname.s), s);
    LIST_cons(msp, l, p, l);

    return l;

} /* end of _ml_P_ProcEnv_uname */

