/* record1.c
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *
 * Create a singleton record.
 */


#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"


/* _ml_RunT_record1 : object -> object
 *
 */
ml_val_t _ml_RunT_record1 (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t    res;

    REC_ALLOC1(msp, res, arg);

    return res;

} /* end of _ml_RunT_record1 */

