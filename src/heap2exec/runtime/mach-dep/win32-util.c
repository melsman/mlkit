/* win32-util.c
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies.
 *
 * win32 specific utility code
 */

#include <windows.h>
#include "ml-osdep.h"
#include "ml-base.h"

int GetPageSize()
{
  SYSTEM_INFO si;

  GetSystemInfo(&si);
  return (int) si.dwPageSize;
}

/* end of win32-util.c */
