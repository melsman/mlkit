/* cfun-list.h
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This file lists the directory library of C functions that are callable by ML.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"POSIX-SysDB"
#define CLIB_VERSION	"1.0"
#define CLIB_DATE	"February 16, 1995"
#endif

CFUNC("getgrgid",  _ml_P_SysDB_getgrgid,  "word -> string * word * string list")
CFUNC("getgrnam",  _ml_P_SysDB_getgrnam,  "string -> string * word * string list")
CFUNC("getpwuid",  _ml_P_SysDB_getpwuid,  "word -> string * word * word * string * string")
CFUNC("getpwnam",  _ml_P_SysDB_getpwnam,  "string -> string * word * word * string * string")

