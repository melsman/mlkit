/* dldummy.c
 *
 * COPYRIGHT (c) 2001 by Lucent Technologies, Bell Laboratories
 */

#include "ml-unixdep.h"
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_Dynload_dldummy : unit -> unit
 *
 * Test function for timing purposes.
 */

ml_val_t _ml_U_Dynload_dldummy (ml_state_t *msp, ml_val_t arg)
{
  return ML_unit;
}
