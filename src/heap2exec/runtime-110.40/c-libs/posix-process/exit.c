/* exit.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_Process_exit : int -> 'a
 *
 * Exit from process
 */
ml_val_t _ml_P_Process_exit (ml_state_t *msp, ml_val_t arg)
{
    Exit (INT_MLtoC(arg));

    /*NOTREACHED*/

} /* end of _ml_P_Process_exit */
