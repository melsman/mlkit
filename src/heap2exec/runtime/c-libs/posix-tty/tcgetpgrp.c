/* tcgetpgrp.c
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

/* _ml_P_TTY_tcgetpgrp : int -> int
 *
 * Get foreground process group id of tty.
 */
ml_val_t _ml_P_TTY_tcgetpgrp (ml_state_t *msp, ml_val_t arg)
{
    int         fd = INT_MLtoC(arg);

    return INT_CtoML(tcgetpgrp(fd));

} /* end of _ml_P_TTY_tcgetpgrp */
