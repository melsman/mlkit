/* cfun-list.h
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This file lists the directory library of C functions that are callable by ML.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"POSIX-ProcEnv"
#define CLIB_VERSION	"1.0"
#define CLIB_DATE	"February 16, 1995"
#endif

CFUNC("getpid",    _ml_P_ProcEnv_getpid,    "unit -> int")
CFUNC("getppid",   _ml_P_ProcEnv_getppid,   "unit -> int")
CFUNC("getuid",    _ml_P_ProcEnv_getuid,    "unit -> word")
CFUNC("geteuid",   _ml_P_ProcEnv_geteuid,   "unit -> word")
CFUNC("getgid",    _ml_P_ProcEnv_getgid,    "unit -> word")
CFUNC("getegid",   _ml_P_ProcEnv_getegid,   "unit -> word")
CFUNC("setuid",    _ml_P_ProcEnv_setuid,    "word -> unit")
CFUNC("setgid",    _ml_P_ProcEnv_setgid,    "word -> unit")
CFUNC("getgroups", _ml_P_ProcEnv_getgroups, "unit -> word list")
CFUNC("getlogin",  _ml_P_ProcEnv_getlogin,  "unit -> string")
CFUNC("getpgrp",   _ml_P_ProcEnv_getpgrp,   "unit -> int")
CFUNC("setsid",    _ml_P_ProcEnv_setsid,    "unit -> int")
CFUNC("setpgid",   _ml_P_ProcEnv_setpgid,   "int * int -> unit")
CFUNC("uname",     _ml_P_ProcEnv_uname,     "unit -> (string * string) list")
CFUNC("sysconf",   _ml_P_ProcEnv_sysconf,   "string -> word")
CFUNC("time",      _ml_P_ProcEnv_time,      "unit -> int")
CFUNC("times",     _ml_P_ProcEnv_times,     "unit -> int * int * int * int * int")
CFUNC("getenv",    _ml_P_ProcEnv_getenv,    "string -> string option")
CFUNC("environ",   _ml_P_ProcEnv_environ,   "unit -> string list")
CFUNC("ctermid",   _ml_P_ProcEnv_ctermid,   "unit -> string")
CFUNC("ttyname",   _ml_P_ProcEnv_ttyname,   "int -> string")
CFUNC("isatty",    _ml_P_ProcEnv_isatty,    "int -> bool")

