/* spin-lock.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "ml-objects.h"
#include "ml-mp.h"
#include "cfun-proto-list.h"


/* _ml_MP_spin_lock:
 */
ml_val_t _ml_MP_spin_lock (ml_state_t *msp, ml_val_t arg)
{
#ifdef MP_SUPPORT
  /* this code is for use the assembly (MIPS.prim.asm) try_lock and lock */
    ml_val_t r;

    REF_ALLOC(msp, r, ML_false);
    return r;
#else
    Die ("ml_spin_lock: no mp support\n");
#endif

} /* end of _ml_MP_spin_lock */
