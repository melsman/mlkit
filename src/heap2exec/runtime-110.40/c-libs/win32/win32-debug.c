/* win32-debug.c
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 *
 * win32 debug support
 */

#include <windows.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"

/* _ml_win32_debug: string -> word
 */
ml_val_t _ml_win32_debug(ml_state_t *msp, ml_val_t arg)
{
  printf("%s",arg);
  return ML_unit;
}

/* end of win32-debug.c */

