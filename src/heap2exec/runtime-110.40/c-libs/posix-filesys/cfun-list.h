/* cfun-list.h
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This file lists the directory library of C functions that are callable by ML.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"POSIX-FileSys"
#define CLIB_VERSION	"1.0"
#define CLIB_DATE	"February 16, 1995"
#endif

CFUNC("osval",	   _ml_P_FileSys_osval,      "string -> int")
CFUNC("chdir",     _ml_P_FileSys_chdir,      "string -> unit")
CFUNC("getcwd",    _ml_P_FileSys_getcwd,     "unit -> string")
CFUNC("openf",     _ml_P_FileSys_openf,      "string * word * word -> int")
CFUNC("umask",     _ml_P_FileSys_umask,      "word -> word")
CFUNC("link",      _ml_P_FileSys_link,       "string * string -> unit")
CFUNC("rename",    _ml_P_FileSys_rename,     "string * string -> unit")
CFUNC("symlink",   _ml_P_FileSys_symlink,    "string * string -> unit")
CFUNC("mkdir",     _ml_P_FileSys_mkdir,      "string * word -> unit")
CFUNC("mkfifo",    _ml_P_FileSys_mkfifo,     "string * word -> unit")
CFUNC("unlink",    _ml_P_FileSys_unlink,     "string -> unit")
CFUNC("rmdir",     _ml_P_FileSys_rmdir,      "string -> unit")
CFUNC("readlink",  _ml_P_FileSys_readlink,   "string -> string")
CFUNC("stat",      _ml_P_FileSys_stat,       "string -> statrep")
CFUNC("lstat",     _ml_P_FileSys_lstat,      "string -> statrep")
CFUNC("fstat",     _ml_P_FileSys_fstat,      "word -> statrep")
CFUNC("access",    _ml_P_FileSys_access,     "string * word -> bool")
CFUNC("chmod",     _ml_P_FileSys_chmod,      "string * word -> unit")
CFUNC("fchmod",    _ml_P_FileSys_fchmod,     "int * word -> unit")
CFUNC("ftruncate", _ml_P_FileSys_ftruncate,  "int * int -> unit")
CFUNC("chown",     _ml_P_FileSys_chown,      "string * word * word -> unit")
CFUNC("fchown",    _ml_P_FileSys_fchown,     "int * word * word -> unit")
CFUNC("utime",     _ml_P_FileSys_utime,      "string * int * int -> unit")
CFUNC("pathconf",  _ml_P_FileSys_pathconf,   "(string * string) -> word option")
CFUNC("fpathconf", _ml_P_FileSys_fpathconf,  "(int * string) -> word option")
CFUNC("opendir",   _ml_P_FileSys_opendir,    "string -> object")
CFUNC("readdir",   _ml_P_FileSys_readdir,    "object -> string")
CFUNC("rewinddir", _ml_P_FileSys_rewinddir,  "object -> unit")
CFUNC("closedir",  _ml_P_FileSys_closedir,   "object -> unit")

