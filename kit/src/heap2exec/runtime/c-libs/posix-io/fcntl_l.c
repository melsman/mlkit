/* fcntl_l.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <fcntl.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_IO_fcntl_l : int * int * flock_rep -> flock_rep
 *    flock_rep = int * int * offset * offset * int
 *
 * Handle record locking.
 */
ml_val_t _ml_P_IO_fcntl_l (ml_state_t *msp, ml_val_t arg)
{
    int              fd = REC_SELINT(arg, 0);
    int              cmd = REC_SELINT(arg, 1);
    ml_val_t         flock_rep = REC_SEL(arg, 2), obj;
    struct flock     flock;
    int              sts;
    
    flock.l_type = REC_SELINT(flock_rep, 0);
    flock.l_whence = REC_SELINT(flock_rep, 1);
    flock.l_start = REC_SELINT(flock_rep, 2);
    flock.l_len = REC_SELINT(flock_rep, 3);
   
    sts = fcntl(fd, cmd, &flock);

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);

    REC_ALLOC5(msp, obj,
	INT_CtoML(flock.l_type),
	INT_CtoML(flock.l_whence), 
	INT_CtoML(flock.l_start),
	INT_CtoML(flock.l_len),
	INT_CtoML(flock.l_pid));

    return obj;

} /* end of _ml_P_IO_fcntl_l */
