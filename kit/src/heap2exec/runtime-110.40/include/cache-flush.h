/* cache-flush.h
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories
 *
 * System dependent includes and macros for flushing the cache.
 */

#ifndef _CACHE_FLUSH_
#define _CACHE_FLUSH_

#ifdef TARGET_MIPS
#  ifdef OPSYS_MACH
#    define INCLUDE_CACHECTL_H	<mips/cachectl.h>
#  else
#    define INCLUDE_CACHECTL_H	<sys/cachectl.h>
#  endif
#endif

#if defined(TARGET_MIPS)
#  include INCLUDE_CACHECTL_H
#  ifdef OPSYS_MACH
#    include <sys/syscall.h>
#    define MIPS_CACHEFLUSH 0x104
#    define FlushICache(addr, size)	\
	(syscall(SYS_sysmips, MIPS_CACHEFLUSH, (addr), (size), BCACHE, 0))
#  else
#    define  FlushICache(addr, size)	\
	(cacheflush((addr), (size), BCACHE))
#  endif

#elif defined(TARGET_X86)
/* 386 & 486 have unified caches and the pentium has hardware consistency */
#  define FlushICache(addr, size)

#elif ((defined(TARGET_RS6000) || defined(TARGET_PPC))&& defined(OPSYS_AIX))
#  include <sys/cache.h>
#  define FlushICache(addr, size)	_sync_cache_range((addr), (size))

#elif (defined(TARGET_SPARC) || defined(TARGET_ALPHA32) || defined(TARGET_HPPA) || defined(OPSYS_MKLINUX))
extern FlushICache (void *addr, int nbytes);


#elif (defined(TARGET_PPC) && (defined(OPSYS_LINUX) || defined(OPSYS_DARWIN) ))
extern FlushICache (void *addr, int nbytes);

#else
#  define FlushICache(addr, size)
#endif

#endif /* !_CACHE_FLUSH_ */

