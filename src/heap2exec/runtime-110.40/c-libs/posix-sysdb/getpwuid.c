/* getpwuid.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <pwd.h>
#include "ml-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_SysDB_getpwuid : word -> string * word * word * string * string
 *
 * Get password file entry by uid.
 */
ml_val_t _ml_P_SysDB_getpwuid (ml_state_t *msp, ml_val_t arg)
{
    struct passwd*    info;
    ml_val_t          pw_name, pw_uid, pw_gid, pw_dir, pw_shell, r;

    info = getpwuid(WORD_MLtoC(arg));
    if (info == NIL(struct passwd *))
        return RAISE_SYSERR(msp, -1);
  
    pw_name = ML_CString (msp, info->pw_name);
    WORD_ALLOC (msp, pw_uid, (Word_t)(info->pw_uid));
    WORD_ALLOC (msp, pw_gid, (Word_t)(info->pw_gid));
    pw_dir = ML_CString (msp, info->pw_dir);
    pw_shell = ML_CString (msp, info->pw_shell);

    REC_ALLOC5(msp, r, pw_name, pw_uid, pw_gid, pw_dir, pw_shell);

    return r;

} /* end of _ml_P_SysDB_getpwuid */
