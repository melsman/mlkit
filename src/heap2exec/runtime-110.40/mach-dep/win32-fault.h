/* win32-fault.h
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 *
 */

extern HANDLE win32_stdin_handle;
extern HANDLE win32_stdout_handle;
extern HANDLE win32_stderr_handle;

extern HANDLE win32_ML_thread_handle;

extern void wait_for_cntrl_c(void);
extern BOOL win32_generic_handler(int code);

extern BOOL win32_isNT;

/* end of win32-fault.h */

