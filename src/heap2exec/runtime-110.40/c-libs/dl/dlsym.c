/* dlsym.c
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

/* _ml_P_Dynload_dlsym : Word32.word * string -> Word32.word
 *
 * Extract symbol from dynamically loaded library.
 */
ml_val_t _ml_U_Dynload_dlsym (ml_state_t *msp, ml_val_t arg)
{
  ml_val_t ml_handle = REC_SEL (arg, 0);
  ml_val_t symname = REC_SEL (arg, 1);
  void *handle = (void *) (WORD_MLtoC (ml_handle));
  void *addr;
  ml_val_t res;

  addr = dlsym (handle, STR_MLtoC (symname));
  
  WORD_ALLOC (msp, res, (Word_t) addr);
  return res;
}
