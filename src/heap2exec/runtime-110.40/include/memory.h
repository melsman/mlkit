/* memory.h
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *
 * An OS independent view of memory.  This supports allocation of
 * memory objects aligned to BIBOP_PAGE_SZB byte boundries (see bibop.h).
 */

#ifndef _MEMORY_
#define _MEMORY_

/* The header of a mem_obj_t structure.  The full representation
 * of this depends on the underlying OS memory system, and thus is
 * abstract.
 */
struct mem_obj_hdr {
    Addr_t	base;	  /* the base address of the object. */
    Addr_t	sizeB;	  /* the object's size (in bytes) */
};

typedef struct mem_obj mem_obj_t;

extern void MEM_InitMemory ();
extern mem_obj_t *MEM_AllocMemObj (Word_t szb);
extern void MEM_FreeMemObj (mem_obj_t *obj);

#define MEMOBJ_BASE(objPtr)	(((struct mem_obj_hdr *)(objPtr))->base)
#define MEMOBJ_SZB(objPtr)	(((struct mem_obj_hdr *)(objPtr))->sizeB)

#ifdef _VM_STATS_
extern long MEM_GetVMSize ();
#endif

#endif /* !_MEMORY_ */

