/* cfun-list.h
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This file lists the directory library of C functions that are callable by ML.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"UNIX-Dynload"
#define CLIB_VERSION	"1.0"
#define CLIB_DATE	"January 1, 2001"
#endif

CFUNC("dlopen",  _ml_U_Dynload_dlopen,  "string option * bool * bool -> Word32.word")
CFUNC("dlsym",   _ml_U_Dynload_dlsym,   "Word32.word * string -> Word32.word")
CFUNC("dlclose", _ml_U_Dynload_dlclose, "Word32.word -> unit")
CFUNC("dlerror", _ml_U_Dynload_dlerror, "unit -> string option")
CFUNC("dldummy", _ml_U_Dynload_dldummy, "unit -> unit")
