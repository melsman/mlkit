/* cfun-list.h
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * This file lists the signals library of C functions that are callable by ML.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"SMLNJ-Signals"
#define CLIB_VERSION	"1.1"
#define CLIB_DATE	"October 29, 1995"
#endif

CFUNC("listSignals",	_ml_Sig_listsigs,	"unit -> sysconst list")
CFUNC("getSigState",	_ml_Sig_getsigstate,	"sysconst -> int")
CFUNC("setSigState",	_ml_Sig_setsigstate,	"(sysconst * int) -> int")
CFUNC("getSigMask",	_ml_Sig_getsigmask,	"unit -> sysconst list option")
CFUNC("setSigMask",	_ml_Sig_setsigmask,	"sysconst list option -> unit")
CFUNC("pause",		_ml_Sig_pause,		"unit -> unit")

