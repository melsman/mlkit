/* clib-list.h
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 */

C_LIBRARY(SMLNJ_RunT_Library)
C_LIBRARY(SMLNJ_Sig_Library)
C_LIBRARY(SMLNJ_Prof_Library)

/* basis libraries */
C_LIBRARY(SMLNJ_Time_Library)
C_LIBRARY(SMLNJ_Date_Library)
C_LIBRARY(SMLNJ_Math_Library)
#if defined(OPSYS_UNIX)
C_LIBRARY(SMLNJ_Sock_Library)
#endif

#ifdef HAS_POSIX_LIBRARIES
C_LIBRARY(POSIX_Error_Library)
C_LIBRARY(POSIX_FileSys_Library)
C_LIBRARY(POSIX_IO_Library)
C_LIBRARY(POSIX_ProcEnv_Library)
C_LIBRARY(POSIX_Process_Library)
C_LIBRARY(POSIX_Signal_Library)
C_LIBRARY(POSIX_SysDB_Library)
C_LIBRARY(POSIX_TTY_Library)
#endif

#ifdef OPSYS_UNIX
C_LIBRARY(POSIX_OS_Library)
#elif defined(OPSYS_WIN32)
C_LIBRARY(WIN32_Library)
C_LIBRARY(WIN32_IO_Library)
C_LIBRARY(WIN32_FileSys_Library)
C_LIBRARY(WIN32_Process_Library)
#endif

#ifdef MP_SUPPORT
C_LIBRARY(SMLNJ_MP_Library)
#endif

#ifdef C_CALLS
C_LIBRARY(SMLNJ_CCalls_Library)
#endif

#ifdef BYTECODE
C_LIBRARY(SMLNJ_BC_Library)
#endif

#ifdef DLOPEN
C_LIBRARY(UNIX_Dynload_Library)
#endif
