/* cfun-list.h
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 *
 * utility win32 C functions
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"WIN32"
#define CLIB_VERSION	"0.1"
#define CLIB_DATE	"October 11, 1996"
#endif

CFUNC("get_const",	   _ml_win32_get_const,	"string -> word32")
CFUNC("get_last_error",    _ml_win32_get_last_error, "unit -> word32")
CFUNC("debug",             _ml_win32_debug, "string -> unit")
