/* getgrnam.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <stdio.h>
#include <grp.h>
#include "ml-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_SysDB_getgrnam : string -> string * word * string list
 *
 * Get group file entry by name.
 */
ml_val_t _ml_P_SysDB_getgrnam (ml_state_t *msp, ml_val_t arg)
{
    struct group*     info;
    ml_val_t          gr_name, gr_gid, gr_mem, r;

    info = getgrnam(STR_MLtoC(arg));
    if (info == NIL(struct group *))
        return RAISE_SYSERR(msp, -1);
  
    gr_name = ML_CString (msp, info->gr_name);
    WORD_ALLOC (msp, gr_gid, (Word_t)(info->gr_gid));
    gr_mem = ML_CStringList(msp, info->gr_mem);

    REC_ALLOC3(msp, r, gr_name, gr_gid, gr_mem);

    return r;

} /* end of _ml_P_SysDB_getgrnam */
