/* read.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <unistd.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_IO_read : (int * int) -> Word8Vector.vector
 *                  fd    nbytes
 *
 * Read the specified number of bytes from the specified file,
 * returning them in a vector.
 */
ml_val_t _ml_P_IO_read (ml_state_t *msp, ml_val_t arg)
{
    int		    fd = REC_SELINT(arg, 0);
    int		    nbytes = REC_SELINT(arg, 1);
    ml_val_t	    vec, res;
    int		    n;

    if (nbytes == 0)
	return ML_string0;

  /* allocate the vector; note that this might cause a GC */
    vec = ML_AllocRaw32 (msp, BYTES_TO_WORDS(nbytes));
    n = read (fd, PTR_MLtoC(char, vec), nbytes);
    if (n < 0)
	return RAISE_SYSERR(msp, n);
    else if (n == 0)
	return ML_string0;

    if (n < nbytes) {
      /* we need to shrink the vector */
	ML_ShrinkRaw32 (msp, vec, BYTES_TO_WORDS(n));
    }

    SEQHDR_ALLOC (msp, res, DESC_string, vec, n);

    return res;

} /* end of _ml_P_IO_read */
