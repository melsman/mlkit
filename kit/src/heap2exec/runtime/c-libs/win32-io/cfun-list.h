/* cfun-list.h
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 *
 * win32 C functions for IO
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"WIN32-IO"
#define CLIB_VERSION	"0.2"
#define CLIB_DATE	"May 22, 1998"
#endif

CFUNC("get_std_handle",\
      _ml_win32_IO_get_std_handle,\
     "word32->word32")
CFUNC("set_file_pointer",\
      _ml_win32_IO_set_file_pointer,\
     "(word32*word32*word32)->word32")
CFUNC("read_vec",\
      _ml_win32_IO_read_vec,\
     "(word32*int)->word8vector.vector")
CFUNC("read_arr",\
      _ml_win32_IO_read_arr,\
     "(word32*word8array.array*int*int)->int")
CFUNC("read_vec_txt",\
      _ml_win32_IO_read_vec_txt,\
     "(word32*int)->char8vector.vector")
CFUNC("read_arr_txt",\
      _ml_win32_IO_read_arr_txt,\
     "(word32*char8array.array*int*int)->int")
CFUNC("close",\
      _ml_win32_IO_close,\
     "word32->unit")
CFUNC("create_file",\
      _ml_win32_IO_create_file,\
     "(string*word32*word32*word32*word32)->word32")
CFUNC("write_vec",\
      _ml_win32_IO_write_vec,\
     "(word32*word8vector.vector*int*int)->int")
CFUNC("write_arr",\
      _ml_win32_IO_write_arr,\
     "(word32*word8array.array*int*int)->int")
CFUNC("write_vec_txt",\
      _ml_win32_IO_write_vec_txt,\
     "(word32*word8vector.vector*int*int)->int")
CFUNC("write_arr_txt",\
      _ml_win32_IO_write_arr_txt,\
     "(word32*word8array.array*int*int)->int")

CFUNC("poll", _ml_win32_OS_poll,"word32 list * word32 list * (Int32.int * int) option -> (word32 list * word32 list)")
     
