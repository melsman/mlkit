/* record-concat.c
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *
 * Concatenation for records.
 */


#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "ml-objects.h"
#include "gc.h"
#include "cfun-proto-list.h"
#include "ml-c.h"



/* _ml_RunT_recordconcat : (object * object) -> object
 *
 */
ml_val_t _ml_RunT_recordconcat (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t    r1 = REC_SEL(arg,0);
    ml_val_t    r2 = REC_SEL(arg,1);

    if (r1 == ML_unit)
	return r2;
    else if (r2 == ML_unit)
	return r1;
    else {
	ml_val_t	res = RecordConcat (msp, r1, r2);

	if (res == ML_unit)
	    return RAISE_ERROR(msp, "recordconcat: not a record");
	else
	    return res;
    }

} /* end of _ml_RunT_recordconcat */

