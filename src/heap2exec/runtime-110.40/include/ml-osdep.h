/* ml-osdep.h
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This file contains definitions to hide a few OS dependencies.  It
 * should be portable across both UNIX and non-UNIX systems (unlike the
 * UNIX specific "ml-unixdep.h" header file).
 *
 *	GETPAGESIZE()		return the machine's pagesize in bytes
 *	PATH_ARC_SEP		the pathname arc separator character
 *	SYSCALL_RET_ERR		this is set, if system calls typically
 *				return an error code status (unlike UNIX,
 *				where the global errno is used).
 */

#ifndef _ML_OSDEP_
#define _ML_OSDEP_

#if defined(OPSYS_UNIX)
#  if (defined(OPSYS_SUNOS) || defined(OPSYS_IRIX4) || defined(OPSYS_LINUX) || defined(OPSYS_AIX) || defined(OPSYS_FREEBSD) || defined(OPSYS_NETBSD) || defined(OPSYS_DARWIN))
#     define GETPAGESIZE()	(getpagesize())
#  else
   /* POSIX 1003.1b interface */
#    include "ml-unixdep.h"
#    ifdef _SC_PAGESIZE
#      define GETPAGESIZE()	(sysconf(_SC_PAGESIZE))
#    else
     /* HPUX engineers can't read specs */
#      define GETPAGESIZE()	(sysconf(_SC_PAGE_SIZE))
#    endif
#  endif

#  define PATH_ARC_SEP	'/'
#  define HAS_GETTIMEOFDAY

#elif defined(OPSYS_MACOS)
#  define PATH_ARC_SEP	':'
#  define SYSCALL_RET_ERR

#elif defined(OPSYS_OS2)
#  define PATH_ARC_SEP	'\\'

#elif defined(OPSYS_WIN32)
#  define PATH_ARC_SEP	'\\'

extern int GetPageSize (void);

#  define GETPAGESIZE()		GetPageSize()
#  define HAS_GETTIMEOFDAY

#endif

#endif /* !_ML_OSDEP_ */

