/* write.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <unistd.h>
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"


/* _ml_P_IO_write : (int * Word8Vector.vector * int) -> int
 *
 * Write the number of bytes of data from the given vector,
 * starting at index 0, to the specified file.  Return the
 * number of bytes written. Assume bounds checks have been done.
 */
ml_val_t _ml_P_IO_write (ml_state_t *msp, ml_val_t arg)
{
    int		fd = REC_SELINT(arg, 0);
    char	*data = REC_SELPTR(char, arg, 1);
    size_t	nbytes = REC_SELINT(arg, 2);
    ssize_t    	n;

    n = write (fd, data, nbytes);

    CHK_RETURN (msp, n)

} /* end of _ml_P_IO_write */
