/* cfun-list.h
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This file lists the directory library of C functions that are callable by ML.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"POSIX-IO"
#define CLIB_VERSION	"1.0"
#define CLIB_DATE	"February 16, 1995"
#endif

CFUNC("osval",	    _ml_P_IO_osval,     "string -> int")
CFUNC("pipe",       _ml_P_IO_pipe,      "unit -> int * int")
CFUNC("dup",        _ml_P_IO_dup,       "int -> int")
CFUNC("dup2",       _ml_P_IO_dup2,      "int * int -> unit")
CFUNC("close",      _ml_P_IO_close,     "int -> unit")
CFUNC("read",       _ml_P_IO_read,      "int * int -> Word8Vector.vector")
CFUNC("readbuf",    _ml_P_IO_readbuf,   "int * Word8Array.array * int -> int")
CFUNC("write",      _ml_P_IO_write,     "int * Word8Vector.vector * int -> int")
CFUNC("writebuf",   _ml_P_IO_writebuf,  "int * Word8Array.array * int * int -> int")
CFUNC("fcntl_d",    _ml_P_IO_fcntl_d,   "int * int -> int")
CFUNC("fcntl_gfd",  _ml_P_IO_fcntl_gfd, "int -> word")
CFUNC("fcntl_sfd",  _ml_P_IO_fcntl_sfd, "int * word -> unit")
CFUNC("fcntl_gfl",  _ml_P_IO_fcntl_gfl, "int -> word * word")
CFUNC("fcntl_sfl",  _ml_P_IO_fcntl_sfl, "int * word -> unit")
CFUNC("fcntl_l",    _ml_P_IO_fcntl_l,   "int * int * flock_rep -> flock_rep")
CFUNC("lseek",      _ml_P_IO_lseek,     "int * int * int -> offset")
CFUNC("fsync",      _ml_P_IO_fsync,     "int -> unit")

