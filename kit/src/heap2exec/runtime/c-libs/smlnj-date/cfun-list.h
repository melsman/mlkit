/* cfun-list.h
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This file lists the directory library of C functions that are callable by ML.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"SMLNJ-Date"
#define CLIB_VERSION	"1.1"
#define CLIB_DATE	"September 5, 1996"
#endif

CFUNC("ascTime",	_ml_Date_asctime,		"")
CFUNC("localTime",	_ml_Date_localtime,		"")
CFUNC("gmTime",		_ml_Date_gmtime,		"")
CFUNC("mkTime",		_ml_Date_mktime,		"")
CFUNC("strfTime",	_ml_Date_strftime,		"")

