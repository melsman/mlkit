/* ftruncate.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */
#include "ml-unixdep.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <sys/types.h>
#include <sys/stat.h>

/* _ml_P_FileSys_ftruncate : (int * int) -> unit
 *                            fd   length
 *
 * Make a directory
 */
ml_val_t _ml_P_FileSys_ftruncate (ml_state_t *msp, ml_val_t arg)
{
    int		    fd = REC_SELINT(arg, 0);
    off_t	    len = REC_SELINT(arg, 1);
    int		    sts;

    sts = ftruncate (fd, len);

    CHK_RETURN_UNIT(msp, sts)

} /* end of _ml_P_FileSys_ftruncate */
