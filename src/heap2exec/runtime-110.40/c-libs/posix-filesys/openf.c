/* openf.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <fcntl.h>
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_FileSys_openf : (string * word * word) -> int
 *                        name     flags  mode
 *
 * Open a file and return the file descriptor.
 */
ml_val_t _ml_P_FileSys_openf (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	    path = REC_SEL(arg, 0);
    int		    flags = REC_SELWORD(arg, 1);
    int		    mode = REC_SELWORD(arg, 2);
    int		    fd;

    fd = open (STR_MLtoC(path), flags, mode);

    CHK_RETURN(msp, fd)

} /* end of _ml_P_FileSys_openf */
