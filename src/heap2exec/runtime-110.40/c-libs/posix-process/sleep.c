/* sleep.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_Process_sleep : int -> int
 *
 * Suspend execution for interval in seconds.
 */
ml_val_t _ml_P_Process_sleep (ml_state_t *msp, ml_val_t arg)
{
    return INT_CtoML(sleep(INT_MLtoC(arg)));

} /* end of _ml_P_Process_sleep */
