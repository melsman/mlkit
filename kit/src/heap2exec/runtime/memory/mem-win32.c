/* mem-win32.c
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies.
 *
 * A simple memory module built on top of vmem alloc/free.
 * This is currently win32 specific.
 */

#if defined(OPSYS_WIN32)
#include <windows.h>
#endif

#include "ml-osdep.h"
#include "ml-base.h"
#include "memory.h"

struct mem_obj {
    Word_t	*base;	  /* the base address of the object. */
    Word_t	sizeB;	  /* the object's size (in bytes) */
    Word_t	*mapBase; /* base address of the mapped region containing */
			  /* the object */
    Addr_t	mapSizeB; /* the size of the mapped region containing  */
			  /* the object */
};

#define MEM_OBJ_SZB  (sizeof(mem_obj_t))

PVT void *alloc_vmem();
PVT void free_vmem(void *);

#define ALLOC_MEMOBJ()		alloc_vmem(MEM_OBJ_SZB)
#define FREE_MEMOBJ		free_vmem

#include "mem-common.ins"

/* alloc_vmem:
 * Allocate some virtual memory.
 */
PVT void *alloc_vmem(int nb)
{
  void *p;

  p = (void *) VirtualAlloc(NULL,
			    nb,
			    MEM_COMMIT|MEM_RESERVE,
			    PAGE_EXECUTE_READWRITE);
  if (p == NULL) {
    Die("VirtualAlloc failed on request of size %lx\n", nb);
  }
  return p;
}

/* free_vmem:
 * Return  memory to OS.
 */
PVT void free_vmem (void *p)
{
  if (!VirtualFree((LPVOID)p,
		   0,
		   MEM_RELEASE)) {
    Die("unable to VirtualFree memory at %lx\n", p);
  }
    
}

PVT status_t MapMemory (mem_obj_t *obj, Addr_t szb)
{
  Addr_t offset, addr;

  if ((addr = (Addr_t) alloc_vmem(szb+BIBOP_PAGE_SZB)) == NIL(Addr_t)) {
    return FAILURE;
  }
  obj->mapBase = (Addr_t *) addr;
  obj->mapSizeB = szb+BIBOP_PAGE_SZB;
  obj->sizeB = szb;
  offset = BIBOP_PAGE_SZB - (addr & (BIBOP_PAGE_SZB-1));
  addr += offset;
  obj->base = (Addr_t *) addr;

  return SUCCESS;
}

PVT void UnmapMemory (mem_obj_t *obj)
{
  free_vmem(obj->mapBase);
  obj->base = obj->mapBase = NULL;
  obj->sizeB = obj->mapSizeB = 0;
}

/* MEM_InitMemory:
 */
void MEM_InitMemory ()
{
    InitMemory();
} /* MEM_InitMemory */

/* end of mem-vmem.c */

