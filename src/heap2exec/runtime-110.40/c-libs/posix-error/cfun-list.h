/* cfun-list.h
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This file lists the directory library of C functions that are callable by ML.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"POSIX-Error"
#define CLIB_VERSION	"1.1"
#define CLIB_DATE	"December 31, 1996"
#endif

CFUNC("errmsg",		_ml_P_Error_errmsg,		"word -> string")
CFUNC("geterror",	_ml_P_Error_geterror,		"word -> sys_const")
CFUNC("listerrors",	_ml_P_Error_listerrors,		"unit -> sys_const list")

