/* max-procs.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-mp.h"
#include "cfun-proto-list.h"


/* _ml_MP_release_proc:
 */
ml_val_t _ml_MP_release_proc (ml_state_t *msp, ml_val_t arg)
{

#ifdef MP_SUPPORT
    MP_ReleaseProc(msp);  /* should not return */
    Die ("_ml_MP_release_proc: call unexpectedly returned\n");
#else
    Die ("_ml_MP_release_proc: no mp support\n");
#endif

} /* end of _ml_MP_release_proc */
