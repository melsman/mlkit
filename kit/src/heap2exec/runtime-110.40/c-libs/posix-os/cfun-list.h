/* cfun-list.h
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * This file lists the directory library of C functions that are callable by ML.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"POSIX-OS"
#define CLIB_VERSION	"1.0"
#define CLIB_DATE	"December 21, 1995"
#endif

CFUNC("poll",		_ml_OS_poll,		"((int * word) list * (int * int) option) -> (int * word) list")
CFUNC("tmpname",	_ml_OS_tmpname,		"unit -> string")

