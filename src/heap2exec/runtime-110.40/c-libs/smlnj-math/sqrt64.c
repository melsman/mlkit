/* sqrt64.c
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 */

#include <math.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

/* _ml_Math_sqrt64:
 */
ml_val_t _ml_Math_sqrt64 (ml_state_t *msp, ml_val_t arg)
{
    double		d = *(PTR_MLtoC(double, arg));
    ml_val_t		res;

    REAL64_ALLOC(msp, res, sqrt(d));

    return res;

} /* end of _ml_Math_sqrt64 */
