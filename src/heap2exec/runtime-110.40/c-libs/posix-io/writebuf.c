/* writebuf.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <unistd.h>
#include "ml-base.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_IO_writebuf : (int * Word8Array.array * int * int) -> int
 *                      fd     data              nbytes start              
 *
 * Write nbytes of data from the given array to the specified file, 
 * starting at the given offset. Assume bounds have been checked.
 */
ml_val_t _ml_P_IO_writebuf (ml_state_t *msp, ml_val_t arg)
{
    int		fd = REC_SELINT(arg, 0);
    ml_val_t	start = REC_SEL(arg, 1);
    size_t	nbytes = REC_SELINT(arg, 2);
    char	*data = STR_MLtoC(start) + REC_SELINT(arg, 3);
    ssize_t    	n;

    n = write (fd, data, nbytes);

    CHK_RETURN (msp, n)

} /* end of _ml_P_IO_writebuf */

