/* alarm.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_Process_alarm : int -> int
 *
 * Set a process alarm clock
 */
ml_val_t _ml_P_Process_alarm (ml_state_t *msp, ml_val_t arg)
{
    return INT_CtoML(alarm(INT_MLtoC(arg)));

} /* end of _ml_P_Process_alarm */
