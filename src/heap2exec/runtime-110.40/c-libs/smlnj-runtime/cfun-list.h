/* cfun-list.h
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * This file lists the directory library of C functions that are callable by ML.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"SMLNJ-RunT"
#define CLIB_VERSION	"1.0"
#define CLIB_DATE	"December 15, 1994"
#endif

CFUNC("argv",		_ml_Proc_argv,		"unit -> string list")
CFUNC("rawArgv",	_ml_Proc_raw_argv,	"unit -> string list")
CFUNC("cmdName",	_ml_Proc_cmd_name,	"unit -> string")
CFUNC("blastIn",	_ml_RunT_blast_in,	"Word8Vector.vector -> 'a")
CFUNC("blastOut",	_ml_RunT_blast_out,	"'a -> Word8Vector.vector")
CFUNC("debug",		_ml_RunT_debug,		"string -> unit")
CFUNC("dummy",		_ml_RunT_dummy,		"string -> unit")
CFUNC("exportHeap",	_ml_RunT_export_heap,	"string -> bool")
CFUNC("exportFn",	_ml_RunT_export_fun,	"(string * (string list -> unit)) -> unit")
CFUNC("gcControl",	_ml_RunT_gc_ctl,	"(string * int ref) list -> unit")
CFUNC("intervalTick",	_ml_RunT_itick,		"unit -> (int * int)")
CFUNC("allocCode",	_ml_RunT_alloc_code,	"")
CFUNC("mkExec",		_ml_RunT_mkexec,	"Word8Vector * int -> object -> object")
CFUNC("mkLiterals",	_ml_RunT_mkliterals,	"Word8Vector.vector -> ovec")
CFUNC("sysInfo",	_ml_RunT_sysinfo,	"string -> string option")
CFUNC("record1",	_ml_RunT_record1,	"object -> object")
CFUNC("recordConcat",	_ml_RunT_recordconcat,	"(object * object) -> object")
CFUNC("setIntTimer",	_ml_RunT_setitimer,	"(int * int) option -> (int * int) option")

