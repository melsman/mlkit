/* mem-mach.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * Memory sub-system for the MACH operating system.
 *
 * The following routines are exported:
 *   void InitMemory ();
 *   mem_obj_t *AllocMemObj (Word_t szb);
 *   void FreeMemObj (mem_obj_t *obj);
 *
 */

#include "ml-unixdep.h"
#include <mach/mach_types.h>
#include "ml-base.h"
#include "memory.h"

#ifndef HAS_VM_ALLOCATE
#  error expected HAS_VM_ALLOCATE
#endif

struct mem_obj {
    Word_t	*base;	  /* the base address of the object. */
    Word_t	sizeB;	  /* the object's size (in bytes) */
    Word_t	*mapBase; /* base address of the mapped region containing */
			  /* the object */
    Word_t	mapSizeB; /* the size of the mapped region containing  */
			  /* the object */
};

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
    Addr_t		addr, offset;
    kern_return_t	sts;

    sts = vm_allocate(task_self(), &addr, szb+BIBOP_PAGE_SZB, TRUE);

    if (sts) {
	errno = sts;
	return FAILURE;
    }

  /* insure BIBOP_PAGE_SZB alignment */
    offset = BIBOP_PAGE_SZB - (addr & (BIBOP_PAGE_SZB-1));
    if (offset != 0) {
      /* align addr and discard unused portions of memory */
	vm_deallocate (task_self(), addr, offset);
	addr += offset;
	vm_deallocate (task_self(), addr+szb, BIBOP_PAGE_SZB-offset);
    }
    else {
	vm_deallocate (task_self(), addr+szb, BIBOP_PAGE_SZB);
    }

    obj->base = (Word_t *)addr;
    obj->sizeB = szb;

    return SUCCESS;

} /* end of MapMemory */

/* UnmapMemory:
 *
 * Unmap a chunk of virtual memory at addr.
 */
PVT void UnmapMemory (mem_obj_t *obj)
{
    kern_return_t	sts;

    sts = vm_deallocate (
		task_self(),
		(vm_address_t)(obj->base),
		(vm_size_t)(obj->sizeB));

    if (sts != KERN_SUCCESS) {
        Die ("error unmapping [%#x, %#x), errno = %d\n",
	    obj->mapBase, (Addr_t)(obj->mapBase) + obj->mapSizeB, errno);
    }

} /* end of UnmapMemory */
