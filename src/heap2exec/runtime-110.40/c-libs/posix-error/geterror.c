/* geterror.c
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * Return the system constant that corresponds to the given error name.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

extern sysconst_tbl_t	_ErrorNo;


/* _ml_P_Error_geterror : int -> sys_const
 */
ml_val_t _ml_P_Error_geterror (ml_state_t *msp, ml_val_t arg)
{
    return ML_SysConst (msp, &_ErrorNo, INT_MLtoC(arg));

} /* end of _ml_P_Error_geterror */

