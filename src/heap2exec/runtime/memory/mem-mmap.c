/* mem-mmap.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * Memory sub-system for systems that provide mmap.
 */

#include "ml-unixdep.h"
#include "ml-osdep.h"
#include <sys/mman.h>
#include INCLUDE_FCNTL_H
#include "ml-base.h"
#include "memory.h"

#if !(defined(HAS_MMAP) || defined(HAS_ANON_MMAP))
#  error expected HAS_MMAP or HAS_ANON_MMAP
#endif

/* protection mode for mmap memory */
#define PROT_ALL	(PROT_READ|PROT_WRITE|PROT_EXEC)

/* flags for mmap */
#ifdef HAS_ANON_MMAP
#  define MMAP_FLGS	(MAP_ANONYMOUS|MAP_PRIVATE)
#else
#  define MMAP_FLGS	MAP_PRIVATE
#endif

#if (defined(HOST_ALPHA32) && (defined(OPSYS_OSF1) || defined(OPSYS_DUNIX)))
  /* To insure that mmap returns a 32-bit address, we need to specify a non-zero
   * address to mmap().  The address 0x2000000 is the location of the text segment,
   * which is a good minimum value since the C stack lives just below this address
   * and mmap will find regions above it.
   */
#  define MMAP_ADDR	(caddr_t)0x2000000
#else
#  define MMAP_ADDR	0
#endif

struct mem_obj {
    Word_t	*base;	  /* the base address of the object. */
    Addr_t	sizeB;	  /* the object's size (in bytes) */
#ifdef HAS_PARTIAL_MUNMAP
#   define	mapBase		base
#   define	mapSizeB	sizeB
#else
    Word_t	*mapBase; /* base address of the mapped region containing */
			  /* the object */
    Addr_t	mapSizeB; /* the size of the mapped region containing  */
			  /* the object */
#endif
};

extern int	errno;

#define ALLOC_MEMOBJ()		NEW_OBJ(mem_obj_t)
#define FREE_MEMOBJ(p)		FREE(p)

#include "mem-common.ins"

/* MEM_InitMemory:
 */
void MEM_InitMemory ()
{
    InitMemory();

} /* MEM_InitMemory */


/* MapMemory:
 *
 * Map a BIBOP_PAGE_SZB aligned chunk of szb bytes of virtual memory.  Return
 * the address of the mapped memory (or NIL on failure).
 */
PVT status_t MapMemory (mem_obj_t *obj, Addr_t szb)
{
    int		fd;
    Addr_t	addr, offset;

#ifdef HAS_ANON_MMAP
    fd = -1;
#else
  /* Note: we use O_RDONLY, because some OS are configured such that /dev/zero
   * is not writable.  This works because we are using MAP_PRIVATE as the
   * mapping mode.
   */
    if ((fd = open("/dev/zero", O_RDONLY)) == -1) {
	Error ("unable to open /dev/zero, errno = %d\n", errno);
	return FAILURE;
    }
#endif

  /* we grab an extra BIBOP_PAGE_SZB bytes to give us some room for alignment */
    addr = (Addr_t) mmap (MMAP_ADDR, szb+BIBOP_PAGE_SZB, PROT_ALL, MMAP_FLGS, fd, 0);
    if (addr == -1) {
	Error ("unable to map %d bytes, errno = %d\n", szb, errno);
#ifndef HAS_ANON_MMAP
	close (fd); /* NOTE: this call clobbers errno */
#endif
	return FAILURE;
    }
#ifndef HAS_ANON_MMAP
    close (fd);
#endif

  /* insure BIBOP_PAGE_SZB alignment */
    offset = BIBOP_PAGE_SZB - (addr & (BIBOP_PAGE_SZB-1));
#ifdef HAS_PARTIAL_MUNMAP
    if (offset != 0) {
      /* align addr and discard unused portions of memory */
	munmap ((void *)addr, offset);
	addr += offset;
	munmap ((void *)(addr+szb), BIBOP_PAGE_SZB-offset);
    }
    else {
	munmap ((void *)(addr+szb), BIBOP_PAGE_SZB);
    }
#else
    obj->mapBase = (Word_t *)addr;
    obj->mapSizeB = szb+BIBOP_PAGE_SZB;
    addr += offset;
#endif
    obj->base = (Word_t *)addr;
    obj->sizeB = szb;

    return SUCCESS;

} /* end of MapMemory */

/* UnmapMemory:
 *
 * Unmap a szb byte chunk of virtual memory at addr.
 */
PVT void UnmapMemory (mem_obj_t *obj)
{
    if (munmap((caddr_t)(obj->mapBase), obj->mapSizeB) == -1) {
	Die ("error unmapping [%#x, %#x), errno = %d\n",
	    obj->mapBase, (Addr_t)(obj->mapBase) + obj->mapSizeB, errno);
    }

} /* end of UnmapMemory */
