/* cfun-list.h
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 * This file lists the directory library of C functions that are callable by ML.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"SMLNJ-Math"
#define CLIB_VERSION	"1.1"
#define CLIB_DATE	"November 1, 1996"
#endif

CFUNC("ctlRoundingMode",	_ml_Math_ctlrndmode,	"int option -> int")
CFUNC("cos64",			_ml_Math_cos64,		"real -> real")
CFUNC("sin64",			_ml_Math_sin64,		"real -> real")
CFUNC("exp64",			_ml_Math_exp64,		"real -> (real * int)")
CFUNC("log64",			_ml_Math_log64,		"real -> real")
CFUNC("sqrt64",			_ml_Math_sqrt64,	"real -> real")
CFUNC("atan64", 		_ml_Math_atan64,        "real -> real")
