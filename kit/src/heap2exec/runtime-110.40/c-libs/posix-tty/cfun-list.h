/* cfun-list.h
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This file lists the directory library of C functions that are callable by ML.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"POSIX-TTY"
#define CLIB_VERSION	"1.0"
#define CLIB_DATE	"August 22, 1995"
#endif

CFUNC("osval",	     _ml_P_TTY_osval,          "string -> int")
CFUNC("tcgetattr",   _ml_P_TTY_tcgetattr,      "int -> termio_rep")
CFUNC("tcsetattr",   _ml_P_TTY_tcsetattr,      "int * int * termio_rep -> unit")
CFUNC("tcsendbreak", _ml_P_TTY_tcsendbreak,    "int * int -> unit")
CFUNC("tcdrain",     _ml_P_TTY_tcdrain,        "int -> unit")
CFUNC("tcflush",     _ml_P_TTY_tcflush,        "int * int -> unit")
CFUNC("tcflow",      _ml_P_TTY_tcflow,         "int * int -> unit")
CFUNC("tcgetpgrp",   _ml_P_TTY_tcgetpgrp,      "int -> int")
CFUNC("tcsetpgrp",   _ml_P_TTY_tcsetpgrp,      "int * int -> unit")

