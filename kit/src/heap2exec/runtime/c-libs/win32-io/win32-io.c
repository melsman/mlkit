/* win32-io.c
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 *
 * interface to win32 io functions
 */

#include <windows.h>

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"

#include "win32-fault.h"

#define EOF_char           '\x01a'           /* ^Z is win32 eof */

/* macro to check if h is a console that hasn't been redirected */
#define IS_CONIN(h) (((h) == win32_stdin_handle) && \
		     (GetFileType(h) == FILE_TYPE_CHAR))

/* _ml_win32_IO_get_std_handle: word32 -> word32
 * interface to win32 GetStdHandle
 */
ml_val_t _ml_win32_IO_get_std_handle(ml_state_t *msp, ml_val_t arg)
{
  Word_t w = WORD_MLtoC(arg);
  HANDLE h = GetStdHandle(w);
  ml_val_t res;

#ifdef WIN32_DEBUG
  SayDebug("getting std handle for %x as %x\n", w, (unsigned int) h);
#endif
  WORD_ALLOC(msp, res, (Word_t)h);
  return res;
}

/* _ml_win32_IO_close: word32 -> unit
 * close a handle
 */
ml_val_t _ml_win32_IO_close(ml_state_t *msp, ml_val_t arg)
{
  HANDLE h = (HANDLE) WORD_MLtoC(arg);
  
  if (CloseHandle(h)) {
    return ML_unit;
  }
#ifdef WIN32_DEBUG
  SayDebug("_ml_win32_IO_close: failing\n");
#endif
  return RAISE_SYSERR(msp,-1);
}


/* _ml_win32_IO_set_file_pointer: (word32 * word32 * word32) -> word32
 *                                 handle   dist     how
 */
ml_val_t _ml_win32_IO_set_file_pointer(ml_state_t *msp, ml_val_t arg)
{
  HANDLE h = (HANDLE) WORD_MLtoC(REC_SEL(arg,0));
  LONG dist = (LONG) WORD_MLtoC(REC_SEL(arg,1));
  DWORD how = (DWORD) WORD_MLtoC(REC_SEL(arg,2));
  Word_t w;
  ml_val_t res;

  w = SetFilePointer(h,dist,NULL,how);
  WORD_ALLOC(msp, res, w);
  return res;
}

/* remove CRs ('\r') from buf of size *np; sets *np to be the new buf size 
 */
PVT rm_CRs(char *buf,int *np)
{
  int i, j = 0;
  int n = *np;

  for (i = 0; i < n; i++) {
    if (buf[i] != '\r') {
      buf[j++] = buf[i];
    }
  }
  *np = j;
}


/* translate CRs ('\r') to newlines ('\n'), removing existing LFs (also '\n').
 * process backspace (BS)
 * sets *np to the new buffer size
 * returns TRUE if the buffer contains the EOF character
 */
PVT bool_t CRLF_EOFscan(char *buf,int *np)
{
  int i, j = 0;
  int n = *np;
  bool_t sawEOF = FALSE;

  for (i = 0; i<n; i++) {
    if (buf[i] == '\r') {             /* translate CRs */
      buf[j++] = '\n';
    } else if (buf[i] == '\b') {      /* process BSes */
      if (j) j--;
    } else if (buf[i] != '\n') {
      if (buf[i] == EOF_char)
	sawEOF = TRUE;
      buf[j++] = buf[i];
    }
  }
  *np = j;
  return sawEOF;
}

/* _ml_win32_IO_read_vec : (word32 * int) -> word8vector.vector
 *                          handle   nbytes
 *
 * Read the specified number of bytes from the specified handle,
 * returning them in a vector.
 *
 * Note: Read operations on console devices do not trap ctrl-C.
 *       ctrl-Cs are placed in the input buffer.
 */
ml_val_t _ml_win32_IO_read_vec(ml_state_t *msp, ml_val_t arg)
{
  HANDLE h = (HANDLE) WORD_MLtoC(REC_SEL(arg, 0));
  DWORD nbytes = (DWORD) REC_SELINT(arg, 1);
  ml_val_t vec;
  DWORD n;

  /* allocate the vector; note that this might cause a GC */
  vec = ML_AllocString (msp, nbytes);
  if (ReadFile(h,PTR_MLtoC(void, vec),nbytes,&n,NULL)) {
    if (n <= nbytes) {
      PTR_MLtoC(ml_val_t, vec)[-1] = MAKE_DESC(n, DTAG_string);
#ifdef WIN32_DEBUG
      if (n == 0)
        SayDebug("_ml_win32_IO_read_vec: eof on device\n");
#endif
      return vec;
    }
  }
#ifdef WIN32_DEBUG
  SayDebug("_ml_win32_IO_read_vec: failing %d %d\n",n,nbytes);
#endif
  return RAISE_SYSERR(msp,-1);
}

PVT void check_cntrl_c(BOOL read_OK,int bytes_read)
{
  /* this is a rude hack */
  /* under NT and default console mode, on 
   *  EOF: read_OK is true, and n > 0
   *   ^C: read_OK is true, and n == 0.  However, the cntrl_c handler is
   *       not always invoked before ReadConsole returns.
   */
  /* under 95 and default console mode, on
   *  EOF: read_OK is true and n is 0
   *   ^C: read_OK is true, n is 0, but handler seems to always have been run
   */
  if (read_OK &&  
      (bytes_read == 0) && 
      win32_isNT) {
    /* guaranteed that a cntrl_c has occurred and has not been reset */
    /* wait for it to happen */
    wait_for_cntrl_c();
  }
}

/* _ml_win32_IO_read_vec_txt : (word32 * int) -> char8vector.vector
 *                             handle   nbytes
 *
 * Read the specified number of bytes from the specified handle,
 * returning them in a vector.
 *
 * reflect changes in _ml_win32_IO_read_arr_txt
 */
ml_val_t _ml_win32_IO_read_vec_txt(ml_state_t *msp, ml_val_t arg)
{
  HANDLE h = (HANDLE) WORD_MLtoC(REC_SEL(arg, 0));
  DWORD nbytes = (DWORD) REC_SELINT(arg, 1);
  ml_val_t vec;
  DWORD	n;
  BOOL flag = FALSE;

  /* allocate the vector; note that this might cause a GC */
  vec = ML_AllocString (msp, nbytes);

  if (IS_CONIN(h)) {
    flag = ReadConsole(h,PTR_MLtoC(void,vec),nbytes,&n,NULL);
    check_cntrl_c(flag,n); 
  } else {
    flag = ReadFile(h,PTR_MLtoC(void,vec),nbytes,&n,NULL);
  }
  if (flag) {
    if (IS_CONIN(h)) {
      if (CRLF_EOFscan((char *)vec,&n)) {
	n = 0;
      }
    } 
    else {
      rm_CRs((char *)vec,&n);
    }
    PTR_MLtoC(ml_val_t, vec)[-1] = MAKE_DESC(n, DTAG_string);
#ifdef WIN32_DEBUG
    SayDebug("_ml_win32_IO_read_vec_txt: read %d\n",n);
#endif
    return vec;
  } else {
    if ((h == win32_stdin_handle) &&             /* input from stdin */
	(GetFileType(h) == FILE_TYPE_PIPE) &&    /* but not console */
        (GetLastError() == ERROR_BROKEN_PIPE)) { /* and pipe broken */
      /* this is an EOF on redirected stdin (ReadFile failed) */
      PTR_MLtoC(ml_val_t, vec)[-1] = MAKE_DESC(0, DTAG_string);
      return vec;
    } 
  }
#ifdef WIN32_DEBUG
  SayDebug("_ml_win32_IO_read_vec_txt: failing on handle %x\n",h);
#endif
  return RAISE_SYSERR(msp,-1);
}

/* _ml_win32_IO_read_arr : (word32*word8array.array*int*int) -> int
 *                          handle buffer           n   start
 *
 * Read n bytes of data from the specified handle into the given array, 
 * starting at start. Return the number of bytes read. Assume bounds
 * have been checked.
 *
 * Note: Read operations on console devices do not trap ctrl-C.
 *       ctrl-Cs are placed in the input buffer.
 */
ml_val_t _ml_win32_IO_read_arr(ml_state_t *msp, ml_val_t arg)
{
  HANDLE h = (HANDLE) WORD_MLtoC(REC_SEL(arg, 0));
  Byte_t *start = REC_SELPTR(Byte_t,arg,1)+REC_SELINT(arg,3);
  DWORD nbytes = (DWORD) REC_SELINT(arg, 2);
  DWORD n;

  if (ReadFile(h,PTR_MLtoC(void,start),nbytes,&n,NULL)) {
#ifdef WIN32_DEBUG
    if (n == 0)
      SayDebug("_ml_win32_IO_read_arr: eof on device\n");
#endif
    return INT_CtoML(n);
  } 
#ifdef WIN32_DEBUG
  SayDebug("_ml_win32_IO_read_arr: failing\n");
#endif
  return RAISE_SYSERR(msp,-1);
}

/* _ml_win32_IO_read_arr_txt : (word32*char8array.array*int*int) -> int
 *                              handle buffer           n   start
 *
 * Read n bytes of data from the specified handle into the given array, 
 * starting at start. Return the number of bytes read. Assume bounds
 * have been checked.
 *
 * reflect changes in _ml_win32_IO_read_vec_txt
 */
ml_val_t _ml_win32_IO_read_arr_txt(ml_state_t *msp, ml_val_t arg)
{
  HANDLE h = (HANDLE) WORD_MLtoC(REC_SEL(arg, 0));
  Byte_t *buf = REC_SELPTR(Byte_t,arg,1)+REC_SELINT(arg,3);
  DWORD	nbytes = (DWORD) REC_SELINT(arg, 2);
  DWORD	n;
  BOOL flag;

  if (IS_CONIN(h)) {
    flag = ReadConsole(h,PTR_MLtoC(void,buf),nbytes,&n,NULL);
    check_cntrl_c(flag,n); 
  } else {
    flag = ReadFile(h,PTR_MLtoC(void,buf),nbytes,&n,NULL);
  }
  if (flag) {
    if (IS_CONIN(h)) {
      if (CRLF_EOFscan((char *)buf,&n)) {
	n = 0;
      }
    } 
    else {
      rm_CRs((char *)buf,&n);
    }
#ifdef WIN32_DEBUG
    SayDebug("_ml_win32_IO_read_arr_txt: eof on device\n");
#endif
    return INT_CtoML(n);
  } else {
    if ((h == win32_stdin_handle) &&             /* input from stdin */
	(GetFileType(h) == FILE_TYPE_PIPE) &&    /* but not console */
        (GetLastError() == ERROR_BROKEN_PIPE)) { /* and pipe broken */
      /* this is an EOF on redirected stdin (ReadFile failed) */
      return INT_CtoML(0);
    } 
  }
#ifdef WIN32_DEBUG
  SayDebug("_ml_win32_IO_read_arr_txt: failing\n");
#endif
  return RAISE_SYSERR(msp,-1);
}


/* _ml_win32_IO_create_file: (string*word32*word32*word32*word32) -> word32 
 *                            name   access share  create attr       handle
 *
 * create file "name" with access, share, create, and attr flags
 */
ml_val_t _ml_win32_IO_create_file(ml_state_t *msp, ml_val_t arg)
{
  char *name = REC_SELPTR(char,arg,0);
  DWORD access = WORD_MLtoC(REC_SEL(arg,1));
  DWORD share = WORD_MLtoC(REC_SEL(arg,2));
  DWORD create = WORD_MLtoC(REC_SEL(arg,3));
  DWORD attr = WORD_MLtoC(REC_SEL(arg,4));
  HANDLE h =  CreateFile(name,access,share,NULL,create,attr,INVALID_HANDLE_VALUE);
  ml_val_t res;

#ifdef WIN32_DEBUG
  if (h == INVALID_HANDLE_VALUE)
    SayDebug("_ml_win32_IO_create_file: failing\n");
#endif
  WORD_ALLOC(msp, res, (Word_t)h);
  return res;
}

/* _ml_win32_IO_write_buf : (word32*word8vector.vector*int*int) -> int
 *                           handle buf                n   offset
 *
 * generic routine for writing n byes from buf to handle starting at offset
 *
 */
ml_val_t _ml_win32_IO_write_buf(ml_state_t *msp, ml_val_t arg)
{
  HANDLE h = (HANDLE) WORD_MLtoC(REC_SEL(arg,0));
  Byte_t *data = REC_SELPTR(Byte_t,arg,1) + REC_SELINT(arg,3);
  DWORD nbytes = (DWORD) REC_SELINT(arg, 2);
  DWORD	n;

#ifdef WIN32_DEBUG
  SayDebug("_ml_win32_IO_write_buf: handle is %x\n", (unsigned int) h);
#endif
  if (WriteFile(h,PTR_MLtoC(void,data),nbytes,&n,NULL)) {
#ifdef WIN32_DEBUG
    if (n == 0)
      SayDebug("_ml_win32_IO_write_buf: eof on device\n");
#endif
    return INT_CtoML(n);
  }
#ifdef WIN32_DEBUG
  SayDebug("_ml_win32_IO_write_buf: failing\n");
#endif
  return RAISE_SYSERR(msp,-1);
}

ml_val_t _ml_win32_IO_write_vec(ml_state_t *msp, ml_val_t arg)
{ 
  return _ml_win32_IO_write_buf(msp,arg);
}

ml_val_t _ml_win32_IO_write_arr(ml_state_t *msp, ml_val_t arg)
{ 
  return _ml_win32_IO_write_buf(msp,arg);
}

ml_val_t _ml_win32_IO_write_vec_txt(ml_state_t *msp, ml_val_t arg)
{ 
  return _ml_win32_IO_write_buf(msp,arg);
}

ml_val_t _ml_win32_IO_write_arr_txt(ml_state_t *msp, ml_val_t arg)
{ 
  return _ml_win32_IO_write_buf(msp,arg);
}

/* end of win32-io.c */
