/* listerrors.c
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * Return the list of system constants that represents the known error
 * codes.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

extern sysconst_tbl_t	_ErrorNo;


/* _ml_P_Error_listerrors : int -> sys_const list
 */
ml_val_t _ml_P_Error_listerrors (ml_state_t *msp, ml_val_t arg)
{
    return ML_SysConstList (msp, &_ErrorNo);

} /* end of _ml_P_Error_listerrors */

