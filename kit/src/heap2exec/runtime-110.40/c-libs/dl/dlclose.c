/* dlclose.c
 *
 * COPYRIGHT (c) 2000 by Lucent Technologies, Bell Laboratories
 */

#include "ml-unixdep.h"
#include <dlfcn.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_Dynload_dlclose : Word32.word -> unit
 *
 * Close dynamically loaded library.
 */
ml_val_t _ml_U_Dynload_dlclose (ml_state_t *msp, ml_val_t ml_handle)
{
  void *handle = (void *) (WORD_MLtoC (ml_handle));

  (void) dlclose (handle);
  
  return ML_unit;
}
