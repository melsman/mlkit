/* cfun-list.h
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 *
 * win32 C functions for IO
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"WIN32-FILESYS"
#define CLIB_VERSION	"0.1"
#define CLIB_DATE	"October 15, 1996"
#endif

CFUNC("find_first_file",\
      _ml_win32_FS_find_first_file,\
      "string->(word32*string option)")
CFUNC("find_next_file",\
      _ml_win32_FS_find_next_file,\
     "word32->(string option)")
CFUNC("find_close",\
      _ml_win32_FS_find_close,\
     "word32->bool")
CFUNC("set_current_directory",\
      _ml_win32_FS_set_current_directory,\
     "string->bool")
CFUNC("get_current_directory",\
      _ml_win32_FS_get_current_directory,\
     "unit->string")
CFUNC("create_directory",\
      _ml_win32_FS_create_directory,\
     "string->bool")
CFUNC("remove_directory",\
      _ml_win32_FS_remove_directory,\
     "string->bool")
CFUNC("get_file_attributes",\
      _ml_win32_FS_get_file_attributes,\
     "string->(word32 option)")
CFUNC("get_file_attributes_by_handle",\
      _ml_win32_FS_get_file_attributes_by_handle,\
     "word32->(word32 option)")
CFUNC("get_full_path_name",\
      _ml_win32_FS_get_full_path_name,\
     "string->string")
CFUNC("get_file_size",\
      _ml_win32_FS_get_file_size,\
     "word32->(word32*word32)")
CFUNC("get_low_file_size",\
      _ml_win32_FS_get_low_file_size,\
     "word32->(word32 option)")
CFUNC("get_low_file_size_by_name",\
      _ml_win32_FS_get_low_file_size_by_name,\
     "string->(word32 option)")
CFUNC("get_file_time",\
      _ml_win32_FS_get_file_time,\
     "string->(int*int*int*int*int*int*int*int) option")
CFUNC("set_file_time",\
      _ml_win32_FS_set_file_time,\
     "(string*(int*int*int*int*int*int*int*int))->bool")
CFUNC("delete_file",\
      _ml_win32_FS_delete_file,\
     "string->bool")
CFUNC("move_file",\
      _ml_win32_FS_move_file,\
     "(string * string)->bool")
CFUNC("get_temp_file_name",\
      _ml_win32_FS_get_temp_file_name,\
     "unit->bool")




