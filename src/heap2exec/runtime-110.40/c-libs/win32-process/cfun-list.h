/* cfun-list.h
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 *
 * win32 C functions for processes
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"WIN32-PROCESS"
#define CLIB_VERSION	"0.2"
#define CLIB_DATE	"May 22, 1998"
#endif

CFUNC("system",_ml_win32_PS_system,"string->word32")
CFUNC("exit_process",_ml_win32_PS_exit_process,"word32->'a")
CFUNC("get_environment_variable",_ml_win32_PS_get_environment_variable,"string->string option")
CFUNC("create_process",_ml_win32_PS_create_process,"string->word32")
CFUNC("wait_for_single_object",_ml_win32_PS_wait_for_single_object,"word32->word32 option")

