/* dlopen.c
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

/* _ml_P_Dynload_dlopen : string * bool * bool -> Word32.word
 *
 * Open a dynamically loaded library.
 */
ml_val_t _ml_U_Dynload_dlopen (ml_state_t *msp, ml_val_t arg)
{
  ml_val_t ml_libname = REC_SEL (arg, 0);
  int lazy = REC_SEL (arg, 1) == ML_true;
  int global = REC_SEL (arg, 2) == ML_true;
  int flag = (lazy ? RTLD_LAZY : RTLD_NOW);
  char *libname = NULL;
  void *handle;
  ml_val_t res;

  if (ml_libname != OPTION_NONE)
    libname = STR_MLtoC (OPTION_get (ml_libname));

  if (global) flag |= RTLD_GLOBAL;

  handle = dlopen (libname, flag);
  
  WORD_ALLOC (msp, res, (Word_t) handle);
  return res;
}
