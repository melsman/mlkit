/* win32-filesys.c
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 *
 * interface to win32 filesys functions
 */

#include <windows.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"

#define TMP_PREFIX "TMP-SMLNJ"

#define IS_DOTDIR(c) ((c)[0] == '.' && (!(c)[1] || ((c)[1] == '.' && !(c)[2])))

static WIN32_FIND_DATA wfd;

static ml_val_t find_next_file(ml_state_t *msp,HANDLE h)
{
  ml_val_t fname_opt,fname;

loop:
  if (FindNextFile(h,&wfd)) {
    if (IS_DOTDIR(wfd.cFileName))
      goto loop;
    fname = ML_CString(msp,wfd.cFileName);
    OPTION_SOME(msp,fname_opt,fname);
  } else
    fname_opt = OPTION_NONE;
  return fname_opt;
}
    
/* _ml_win32_FS_find_next_file: word32 -> (string option)
 */
ml_val_t _ml_win32_FS_find_next_file(ml_state_t *msp, ml_val_t arg)
{
  HANDLE h = (HANDLE) WORD_MLtoC(arg);

  return find_next_file(msp,h);
}

/* _ml_win32_FS_find_first_file: string -> (word32 * string option)
 */
ml_val_t _ml_win32_FS_find_first_file(ml_state_t *msp, ml_val_t arg)
{
  HANDLE h = FindFirstFile(STR_MLtoC(arg),&wfd);
  ml_val_t fname_opt, fname, w, res;

  if (h != INVALID_HANDLE_VALUE) {
    if (IS_DOTDIR(wfd.cFileName))
      fname_opt = find_next_file(msp,h);
    else {
      fname = ML_CString(msp,wfd.cFileName);
      OPTION_SOME(msp,fname_opt,fname);
    }
  } else
    fname_opt = OPTION_NONE;
  WORD_ALLOC(msp, w, (Word_t)h);
  REC_ALLOC2(msp,res,w,fname_opt);
  return res;
}

/* _ml_win32_FS_find_close: word32 -> bool
 */
ml_val_t _ml_win32_FS_find_close(ml_state_t *msp, ml_val_t arg)
{
  return FindClose((HANDLE)WORD_MLtoC(arg)) ? ML_true : ML_false;
}

/* _ml_win32_FS_set_current_directory: string -> bool
 */
ml_val_t _ml_win32_FS_set_current_directory(ml_state_t *msp, ml_val_t arg)
{
  return SetCurrentDirectory(STR_MLtoC(arg)) ? ML_true : ML_false;
}

/* _ml_win32_FS_get_current_directory: unit -> string
 */
ml_val_t _ml_win32_FS_get_current_directory(ml_state_t *msp, ml_val_t arg)
{
  char buf[MAX_PATH];
  DWORD r = GetCurrentDirectory(MAX_PATH,buf);

  if (r == 0 || r > MAX_PATH) {
    return RAISE_SYSERR(msp,-1);
  }
  return ML_CString(msp,buf);
}


/* _ml_win32_FS_create_directory: string -> bool
 */
ml_val_t _ml_win32_FS_create_directory(ml_state_t *msp, ml_val_t arg)
{
  return CreateDirectory(STR_MLtoC(arg),NULL) ? ML_true : ML_false;
}

/* _ml_win32_FS_remove_directory: string -> bool
 */
ml_val_t _ml_win32_FS_remove_directory(ml_state_t *msp, ml_val_t arg)
{
  return RemoveDirectory(STR_MLtoC(arg)) ? ML_true : ML_false;
}

/* _ml_win32_FS_get_file_attributes: string -> (word32 option)
 */
ml_val_t _ml_win32_FS_get_file_attributes(ml_state_t *msp, ml_val_t arg)
{
  DWORD w = GetFileAttributes(STR_MLtoC(arg));
  ml_val_t res, ml_w;

  if (w != 0xffffffff) {
#ifdef DEBUG_WIN32
    printf("_ml_win32_FS_get_file_attributes: returning file attrs for <%s> as SOME %x\n",STR_MLtoC(arg),w);
#endif
    WORD_ALLOC(msp,ml_w,w);
    OPTION_SOME(msp,res,ml_w);
  } else {
#ifdef DEBUG_WIN32
    printf("returning NONE as attrs for <%s>\n",STR_MLtoC(arg));
#endif
    res = OPTION_NONE;
  }
  return res;
}

/* _ml_win32_FS_get_file_attributes_by_handle: word32 -> (word32 option)
 */
ml_val_t _ml_win32_FS_get_file_attributes_by_handle(ml_state_t *msp, ml_val_t arg)
{
  BY_HANDLE_FILE_INFORMATION bhfi;
  ml_val_t ml_w, res;

  if (GetFileInformationByHandle((HANDLE)WORD_MLtoC(arg),&bhfi)) {
    WORD_ALLOC(msp,ml_w,bhfi.dwFileAttributes);
    OPTION_SOME(msp,res,ml_w);
  } else {
    res = OPTION_NONE;
  }
  return res;
}

/* _ml_win32_FS_get_full_path_name: string -> string
 */
ml_val_t _ml_win32_FS_get_full_path_name(ml_state_t *msp, ml_val_t arg)
{
  char buf[MAX_PATH], *dummy;
  DWORD r;
  ml_val_t res;

  r = GetFullPathName(STR_MLtoC(arg),MAX_PATH,buf,&dummy);
  if (r == 0 | r > MAX_PATH) {
    return  RAISE_SYSERR(msp,-1);
  }
  res = ML_CString(msp,buf);
  return res;
}

/* _ml_win32_FS_get_file_size: word32 -> (word32 * word32)
 */
ml_val_t _ml_win32_FS_get_file_size(ml_state_t *msp, ml_val_t arg)
{
  DWORD lo,hi;
  ml_val_t ml_lo, ml_hi, res;

  lo = GetFileSize((HANDLE)WORD_MLtoC(arg),&hi);
  WORD_ALLOC(msp,ml_lo,lo);
  WORD_ALLOC(msp,ml_hi,hi);
  REC_ALLOC2(msp,res,ml_hi,ml_lo);
  return res;
}

/* _ml_win32_FS_get_low_file_size: word32 -> (word32 option)
 */
ml_val_t _ml_win32_FS_get_low_file_size(ml_state_t *msp, ml_val_t arg)
{
  DWORD lo;
  ml_val_t ml_lo, res;

  lo = GetFileSize((HANDLE)WORD_MLtoC(arg),NULL);
  if (lo != 0xffffffff) {
    WORD_ALLOC(msp,ml_lo,lo);
    OPTION_SOME(msp,res,ml_lo);
  } else {
    res = OPTION_NONE;
  }
  return res;
}

/* _ml_win32_FS_get_low_file_size_by_name: string -> (word32 option)
 */
ml_val_t _ml_win32_FS_get_low_file_size_by_name(ml_state_t *msp, ml_val_t arg)
{
  HANDLE h;
  ml_val_t res = OPTION_NONE;

  h = CreateFile(STR_MLtoC(arg),0,0,NULL,
		 OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,INVALID_HANDLE_VALUE);
  if (h != INVALID_HANDLE_VALUE) {
    DWORD lo;
    ml_val_t ml_lo;

    lo = GetFileSize(h,NULL);
    CloseHandle(h);
    if (lo != 0xffffffff) {
      WORD_ALLOC(msp,ml_lo,lo);
      OPTION_SOME(msp,res,ml_lo);
    }
  }
  return res;
}

#define REC_ALLOC8(msp, r, a, b, c, d, e, f, g, h)	{	\
	ml_state_t	*__msp = (msp);				\
	ml_val_t	*__p = __msp->ml_allocPtr;		\
	*__p++ = MAKE_DESC(8, DTAG_record);			\
	*__p++ = (a);						\
	*__p++ = (b);						\
	*__p++ = (c);						\
	*__p++ = (d);						\
	*__p++ = (e);						\
	*__p++ = (f);						\
	*__p++ = (g);						\
	*__p++ = (h);						\
	(r) = PTR_CtoML(__msp->ml_allocPtr + 1);		\
	__msp->ml_allocPtr = __p;				\
    }

/* _ml_win32_FS_get_file_time: 
 *   string -> (int * int * int * int * int * int * int * int) option
 *              year  month wday  day   hour  min   sec   ms
 */
ml_val_t _ml_win32_FS_get_file_time(ml_state_t *msp, ml_val_t arg)
{
  HANDLE h;
  ml_val_t res = OPTION_NONE;

  h = CreateFile(STR_MLtoC(arg),0,0,NULL,
		 OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,INVALID_HANDLE_VALUE);
  if (h != INVALID_HANDLE_VALUE) {
    FILETIME ft;

    if (GetFileTime(h,NULL,NULL,&ft)) {  /* request time of "last write" */
      SYSTEMTIME st;
    
      CloseHandle(h);
      if (FileTimeToSystemTime(&ft,&st)) {
	ml_val_t rec;

	REC_ALLOC8(msp,rec,
		   INT_CtoML((int)st.wYear),
		   INT_CtoML((int)st.wMonth),
		   INT_CtoML((int)st.wDayOfWeek),
		   INT_CtoML((int)st.wDay),
		   INT_CtoML((int)st.wHour),
		   INT_CtoML((int)st.wMinute),
		   INT_CtoML((int)st.wSecond),
		   INT_CtoML((int)st.wMilliseconds));
	OPTION_SOME(msp,res,rec);
      }
    }
  }
  return res;
}

/* _ml_win32_FS_set_file_time: 
 *   (string * (int * int * int * int * int * int * int * int) option) -> bool
 *              year  month wday  day   hour  min   sec   ms
 */
ml_val_t _ml_win32_FS_set_file_time(ml_state_t *msp, ml_val_t arg)
{
  HANDLE	h;
  ml_val_t	res = ML_false;
  ml_val_t	fname = REC_SEL(arg,0);
  ml_val_t	time_rec = REC_SEL(arg,1);

  h = CreateFile (
	STR_MLtoC(fname), GENERIC_WRITE, 0 ,NULL,
	OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, INVALID_HANDLE_VALUE);

  if (h != INVALID_HANDLE_VALUE) {
    FILETIME ft;
    SYSTEMTIME st;

    st.wYear = REC_SELINT(time_rec,0);
    st.wMonth = REC_SELINT(time_rec,1);
    st.wDayOfWeek = REC_SELINT(time_rec,2);
    st.wDay = REC_SELINT(time_rec,3);
    st.wHour = REC_SELINT(time_rec,4);
    st.wMinute = REC_SELINT(time_rec,5);
    st.wSecond = REC_SELINT(time_rec,6);
    st.wMilliseconds = REC_SELINT(time_rec,7);
    
    if (SystemTimeToFileTime(&st,&ft) && SetFileTime(h,&ft,NULL,NULL)) {
      res = ML_true;
    }
    
    CloseHandle(h);
  }
  return res;
}

/* _ml_win32_FS_delete_file: string->bool
 */
ml_val_t _ml_win32_FS_delete_file(ml_state_t *msp, ml_val_t arg)
{
  return DeleteFile(STR_MLtoC(arg)) ? ML_true : ML_false;
}

/* _ml_win32_FS_move_file: (string * string)->bool
 */
ml_val_t _ml_win32_FS_move_file(ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	f1 = REC_SEL(arg, 0);
    ml_val_t	f2 = REC_SEL(arg, 1);

    if (MoveFile (STR_MLtoC(f1), STR_MLtoC(f2)))
	return ML_true;
    else
	return ML_false;
}

/* _ml_win32_FS_get_temp_file_name: unit -> string option
 */
ml_val_t _ml_win32_FS_get_temp_file_name(ml_state_t *msp, ml_val_t arg)
{
  ml_val_t res = OPTION_NONE;
  char name_buf[MAX_PATH];
  char path_buf[MAX_PATH];
  DWORD pblen;

  pblen = GetTempPath(MAX_PATH,path_buf);
  if ((pblen <= MAX_PATH) && 
      (GetTempFileName(path_buf,TMP_PREFIX,0,name_buf) != 0)) {
    ml_val_t tfn = ML_CString(msp,name_buf);
    
    OPTION_SOME(msp,res,tfn);
  }
  return res;
}

/* end of win32-filesys.c */
