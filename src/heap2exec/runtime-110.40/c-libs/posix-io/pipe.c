/* pipe.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_IO_pipe : unit -> int * int
 *
 * Create a pipe and return its input and output descriptors.
 */
ml_val_t _ml_P_IO_pipe (ml_state_t *msp, ml_val_t arg)
{
    int         fds[2];

    if (pipe(fds) == -1)
        return RAISE_SYSERR(msp, -1);
    else {
        ml_val_t        obj;
        REC_ALLOC2 (msp, obj, INT_CtoML(fds[0]), INT_CtoML(fds[1]));
        return obj;
    }

} /* end of _ml_P_IO_pipe */
