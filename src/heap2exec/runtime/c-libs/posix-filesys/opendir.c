/* opendir.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <sys/types.h>
#include <dirent.h>
#include "ml-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_FileSys_opendir : string -> object
 *
 * Open and return a directory stream.
 */
ml_val_t _ml_P_FileSys_opendir (ml_state_t *msp, ml_val_t arg)
{
    DIR      *dir;
    
    dir = opendir(PTR_MLtoC(char, arg));
    if (dir == NIL(DIR *))
	return RAISE_SYSERR(msp, -1);
    else
	return PTR_CtoML(dir);

} /* end of _ml_P_FileSys_opendir */
