/* fcntl_gfl.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <fcntl.h>
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_IO_fcntl_gfl : int -> word * word
 *
 * Get file status flags and file access modes.
 */
ml_val_t _ml_P_IO_fcntl_gfl (ml_state_t *msp, ml_val_t arg)
{
    int             fd = INT_MLtoC(arg);
    int             flag;
    ml_val_t        flags, mode, obj;

    flag = fcntl(fd, F_GETFD);

    if (flag < 0)
	return RAISE_SYSERR(msp, flag);

    WORD_ALLOC (msp, flags, (flag & (~O_ACCMODE)));
    WORD_ALLOC (msp, mode, (flag & O_ACCMODE));
    REC_ALLOC2(msp, obj, flags, mode);

    return obj;

} /* end of _ml_P_IO_fcntl_gfl */
