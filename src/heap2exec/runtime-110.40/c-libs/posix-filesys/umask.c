/* umask.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <sys/types.h>
#include <sys/stat.h>
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_FileSys_umask : word -> word
 *
 * Set and get file creation mask
 * Assumes umask never fails.
 */
ml_val_t _ml_P_FileSys_umask (ml_state_t *msp, ml_val_t arg)
{
    mode_t		omask;
    ml_val_t            p;

    omask = umask(WORD_MLtoC(arg));
    WORD_ALLOC (msp, p, (Word_t)omask);

    return p;

} /* end of _ml_P_FileSys_umask */
