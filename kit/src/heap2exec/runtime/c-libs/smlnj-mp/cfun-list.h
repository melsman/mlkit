/* cfun-list.h
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * This file lists the directory library of C functions that are callable by ML.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"SMLNJ-MP"
#define CLIB_VERSION	"1.0"
#define CLIB_DATE	"December 18, 1994"
#endif

CFUNC("acquire_proc",	_ml_MP_acquire_proc,	"")
CFUNC("max_procs",	_ml_MP_max_procs,	"")
CFUNC("release_proc",	_ml_MP_release_proc,	"")
CFUNC("spin_lock",	_ml_MP_spin_lock,	"")

