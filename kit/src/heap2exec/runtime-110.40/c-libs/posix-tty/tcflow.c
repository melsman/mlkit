/* tcflow.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <termios.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_TTY_tcflow : int * int -> unit
 *
 * Suspend transmission or receipt of data.
 */
ml_val_t _ml_P_TTY_tcflow (ml_state_t *msp, ml_val_t arg)
{
    int         sts;

    sts = tcflow(REC_SELINT(arg, 0),REC_SELINT(arg, 1));

    CHK_RETURN_UNIT(msp, sts)

} /* end of _ml_P_TTY_tcflow */
