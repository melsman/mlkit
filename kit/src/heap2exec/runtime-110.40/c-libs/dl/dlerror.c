/* dlerror.c
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

/* _ml_P_Dynload_dlerror : unit -> string option
 *
 * Extract error after unsuccessful dlopen/dlsym/dlclose.
 */
ml_val_t _ml_U_Dynload_dlerror (ml_state_t *msp, ml_val_t ml_handle)
{
  char *e = dlerror ();
  ml_val_t r, s;

  if (e == NULL)
    r = OPTION_NONE;
  else {
    s = ML_CString (msp, e);
    OPTION_SOME (msp, r, s);
  }
  return r;
}
