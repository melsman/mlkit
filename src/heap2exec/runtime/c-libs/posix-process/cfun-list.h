/* cfun-list.h
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This file lists the directory library of C functions that are callable by ML.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"POSIX-Process"
#define CLIB_VERSION	"1.0"
#define CLIB_DATE	"February 16, 1995"
#endif

CFUNC("osval",  _ml_P_Process_osval,    "string -> int")
CFUNC("fork",   _ml_P_Process_fork,     "unit -> int")
CFUNC("exec",   _ml_P_Process_exec,     "(string * string list) -> 'a")
CFUNC("exece",  _ml_P_Process_exece,    "(string * string list * string list) -> 'a")
CFUNC("execp",   _ml_P_Process_execp,   "(string * string list) -> 'a")
CFUNC("waitpid", _ml_P_Process_waitpid, "int * word -> int * int * int")
CFUNC("exit",    _ml_P_Process_exit,    "int -> 'a")
CFUNC("kill",    _ml_P_Process_kill,    "int * int -> unit")
CFUNC("alarm",   _ml_P_Process_alarm,   "int -> int")
CFUNC("pause",   _ml_P_Process_pause,   "unit -> unit")
CFUNC("sleep",   _ml_P_Process_sleep,   "int -> int")

