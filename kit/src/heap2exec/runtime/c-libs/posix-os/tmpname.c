/* exit.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 */

#include <stdio.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

/* _ml_OS_tmpname:
 */
ml_val_t _ml_OS_tmpname (ml_state_t *msp, ml_val_t arg)
{
    char	buf[L_tmpnam];

    tmpnam (buf);

    return ML_CString (msp, buf);

} /* end of _ml_OS_tmpname */

