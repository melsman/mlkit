/* waitpid.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <sys/wait.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_Process_waitpid : int * word -> int * int * int
 *
 * Wait for child processes to stop or terminate
 */
ml_val_t _ml_P_Process_waitpid (ml_state_t *msp, ml_val_t arg)
{
    int       pid;
    int       status, how, val;
    ml_val_t  r;

    pid = waitpid(REC_SELINT(arg, 0), &status, REC_SELWORD(arg,1));

    if (pid < 0)
	return RAISE_SYSERR(msp, pid);

    if (WIFEXITED(status)) {
	how = 0;
	val = WEXITSTATUS(status);
    }
    else if (WIFSIGNALED(status)) {
	how = 1;
	val = WTERMSIG(status);
    }
    else if (WIFSTOPPED(status)) {
	how = 2;
	val = WSTOPSIG(status);
    }
    else
	return RAISE_ERROR(msp, "unknown child status");

    REC_ALLOC3(msp, r, INT_CtoML(pid), INT_CtoML(how), INT_CtoML(val));

    return r;

} /* end of _ml_P_Process_waitpid */
