/* win32-process.c
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 *
 * interface to win32 process functions
 */

#include <windows.h>
#include <process.h>
#include <stdlib.h>

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"


/* _ml_win32_PS_create_process : string -> word32
 * 
 * Note: This function returns the handle to the created process
 *       This handle will need to be freed before the system releases
 *       the memory associated to the process.
 *       We will take care of this in the wait_for_single_object
 *       call. This is for the time being only used by CML.
 *       It could also cause problems later on.
 */
ml_val_t _ml_win32_PS_create_process(ml_state_t *msp, ml_val_t arg)
{
  char *str = STR_MLtoC(,arg);
  PROCESS_INFORMATION pi;
  STARTUPINFO si;
  ml_val_t res;
  BOOL fSuccess;
  ZeroMemory (&si,sizeof(si));
  si.cb = sizeof(si);
  fSuccess = CreateProcess (NULL,str,NULL,NULL,FALSE,0,NULL,NULL,&si,&pi);
  if (fSuccess) {
    HANDLE hProcess = pi.hProcess;
    CloseHandle (pi.hThread);
    WORD_ALLOC (msp,res,(Word_t)hProcess);
    return res;
  }
  WORD_ALLOC (msp,res,(Word_t)0);
  return res;
}

ml_val_t _ml_win32_PS_wait_for_single_object(ml_state_t *msp, ml_val_t arg)
{
  HANDLE hProcess = (HANDLE) WORD_MLtoC (arg);
  DWORD exit_code;
  int res;
  ml_val_t p,obj;
  res = WaitForSingleObject (hProcess,0);
  if (res==WAIT_TIMEOUT || res==WAIT_FAILED) {
    /* information is not ready, or error */
    obj = OPTION_NONE;
  }
  else { 
    /* WAIT_OBJECT_0 ... done, finished */
    /* get info and return SOME(exit_status) */
    GetExitCodeProcess (hProcess,&exit_code);
    CloseHandle (hProcess);   /* decrease ref count */
    WORD_ALLOC (msp,p,(Word_t)exit_code);
    OPTION_SOME(msp,obj,p);
  }
  return obj;
}  
    

/* _ml_win32_PS_system : string -> word32
 *                       command
 *
 */
ml_val_t _ml_win32_PS_system(ml_state_t *msp, ml_val_t arg)
{
  int ret = system(STR_MLtoC(arg));
  ml_val_t res;

  WORD_ALLOC(msp, res, (Word_t)ret);
  return res;
}

/* _ml_win32_PS_exit_process : word32 -> 'a
 *                             exit code
 *
 */
void _ml_win32_PS_exit_process(ml_state_t *msp, ml_val_t arg)
{
  ExitProcess((UINT)WORD_MLtoC(arg));
}

/* _ml_win32_PS_get_environment_variable : string -> string option
 *                                         var
 *
 */
ml_val_t _ml_win32_PS_get_environment_variable(ml_state_t *msp, ml_val_t arg)
{
#define GEV_BUF_SZ 4096
  char buf[GEV_BUF_SZ];
  int ret = GetEnvironmentVariable(STR_MLtoC(arg),buf,GEV_BUF_SZ);
  ml_val_t ml_s,res = OPTION_NONE;

  if (ret > GEV_BUF_SZ) {
    return RAISE_SYSERR(msp,-1);
  }
  if (ret > 0) {
    ml_s = ML_CString(msp,buf);
    OPTION_SOME(msp,res,ml_s);
  }
  return res;
#undef GEV_BUF_SZ
}

/* end of win32-process.c */
