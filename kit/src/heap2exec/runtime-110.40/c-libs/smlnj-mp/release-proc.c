/* release-proc.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-mp.h"
#include "cfun-proto-list.h"


/* _ml_MP_max_procs:
 */
ml_val_t _ml_MP_max_procs (ml_state_t *msp, ml_val_t arg)
{
#ifdef MP_SUPPORT
    return INT_CtoML(MP_MaxProcs ());
#else
    Die ("_ml_MP_max_procs: no mp support\n");
#endif

} /* end of _ml_MP_max_procs */
