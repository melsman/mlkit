/* cfun-list.h
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * This file lists the directory library of C functions that are callable by ML.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"SMLNJ-Prof"
#define CLIB_VERSION	"1.0"
#define CLIB_DATE	"December 15, 1994"
#endif

CFUNC("setTimer",	_ml_Prof_setptimer,	"bool -> unit")
CFUNC("getQuantum",	_ml_Prof_getpquantum,	"unit -> int")
CFUNC("setTimeArray",	_ml_Prof_setpref,	"word array option -> unit")
