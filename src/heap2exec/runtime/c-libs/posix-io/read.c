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
    ml_val_t	    vec;
    int		    n;

  /* allocate the vector; note that this might cause a GC */
    vec = ML_AllocString (msp, nbytes);
    n = read (fd, PTR_MLtoC(char, vec), nbytes);
    if (n < 0)
	return RAISE_SYSERR(msp, n);

    if (n < nbytes) {
      /* we need to correct the length in the descriptor */
	PTR_MLtoC(ml_val_t, vec)[-1] = MAKE_DESC(n, DTAG_string);
    }

    return vec;

} /* end of _ml_P_IO_read */
