/* isatty.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_ProcEnv_isatty: int -> bool
 *
 * Is file descriptor associated with a terminal device
 */
ml_val_t _ml_P_ProcEnv_isatty (ml_state_t *msp, ml_val_t arg)
{
    return (isatty(INT_MLtoC(arg)) ? ML_true : ML_false);

} /* end of _ml_P_ProcEnv_isatty */

