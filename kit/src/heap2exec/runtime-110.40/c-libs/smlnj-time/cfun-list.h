/* cfun-list.h
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * This file lists the directory library of C functions that are callable by ML.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"SMLNJ-Time"
#define CLIB_VERSION	"1.0"
#define CLIB_DATE	"December 16, 1994"
#endif

CFUNC("gettime",	_ml_Time_gettime,		"")
CFUNC("timeofday",	_ml_Time_timeofday,		"")

