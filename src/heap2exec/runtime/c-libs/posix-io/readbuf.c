/* readbuf.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include <unistd.h>
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_IO_readbuf : (int * Word8Array.array * int * int) -> int
 *                     fd    data               nbytes start
 *
 * Read nbytes of data from the specified file into the given array, 
 * starting at start. Return the number of bytes read. Assume bounds
 * have been checked.
 */
ml_val_t _ml_P_IO_readbuf (ml_state_t *msp, ml_val_t arg)
{
    int		fd = REC_SELINT(arg, 0);
    char	*start = REC_SELPTR(char, arg, 1) + REC_SELINT(arg, 3);
    int		nbytes = REC_SELINT(arg, 2);
    int		n;

    n = read (fd, start, nbytes);

    CHK_RETURN (msp, n)

} /* end of _ml_P_IO_readbuf */

