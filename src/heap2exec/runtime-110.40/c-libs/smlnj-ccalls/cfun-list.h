/* cfun-list.h
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * This file lists the directory library of C functions that are callable by ML.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"SMLNJ-CCalls"
#define CLIB_VERSION	"0.0"
#define CLIB_DATE	"March 3, 1995"
#endif

CFUNC("c_call",		ml_c_call,		"")
CFUNC("datumMLtoC",	ml_datumMLtoC,		"")
CFUNC("datumCtoML",	ml_datumCtoML,		"")

#include "cutil-cfuns.h"

