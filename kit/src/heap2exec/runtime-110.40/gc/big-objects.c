/* big-objects.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * Code for managing big-object regions.
 */

#include "ml-base.h"
#include "memory.h"
#include "heap.h"
#include "heap-monitor.h"
#include <string.h>

#ifdef BO_DEBUG
/* PrintRegionMap:
 */
void PrintRegionMap (bigobj_region_t *r)
{
    bigobj_desc_t	*dp, *dq;
    int			i;

    SayDebug ("[%d] %d/%d, @%#x: ", r->minGen, r->nFree, r->nPages, r->firstPage);
    for (i = 0, dq = NIL(bigobj_desc_t *);  i < r->nPages;  i++) {
	dp = r->objMap[i];
	if (dp != dq) {
	    SayDebug ("|");
	    dq = dp;
	}
	if (BO_IS_FREE(dp))
	    SayDebug ("_");
	else
	    SayDebug ("X");
    }
    SayDebug ("|\n");

} /* end of PrintRegionMap */
#endif


/* BO_AllocRegion:
 *
 * Allocate a big object region that is large enough to hold an object of at
 * least szB bytes.  It returns the descriptor for the free big-object that
 * is the region.
 * NOTE: it does not mark the BIBOP entries for the region; this should be
 * done by the caller.
 */
bigobj_desc_t *BO_AllocRegion (heap_t *heap, Addr_t szB)
{
    int		    npages, oldNpages, i;
    Addr_t	    hdrSzB, memObjSzB;
    bigobj_region_t *region;
    mem_obj_t	    *memObj;
    bigobj_desc_t   *desc;

  /* compute the memory object size.
   * NOTE: there probably is a closed form for this, but I'm too lazy
   * to try to figure it out.
   */
    npages = ROUNDUP(szB, BIGOBJ_PAGE_SZB) >> BIGOBJ_PAGE_SHIFT;
    do {
	oldNpages = npages;
	hdrSzB = ROUNDUP(BOREGION_HDR_SZB(npages), BIGOBJ_PAGE_SZB);
	szB = (npages << BIGOBJ_PAGE_SHIFT);
	memObjSzB = RND_MEMOBJ_SZB(hdrSzB+szB);
	memObjSzB = (memObjSzB < MIN_BOREGION_SZB) ? MIN_BOREGION_SZB : memObjSzB;
	npages = (memObjSzB - hdrSzB) >> BIGOBJ_PAGE_SHIFT;
    } while (npages != oldNpages);

    if ((memObj = MEM_AllocMemObj (memObjSzB)) == NIL(mem_obj_t *))
	Die ("unable to allocate memory object for bigobject region");
    region = (bigobj_region_t *)MEMOBJ_BASE(memObj);

    if ((desc = NEW_OBJ(bigobj_desc_t)) == NIL(bigobj_desc_t *))
	Die ("unable to allocate big-object descriptor");

  /* initialize the region header */
    region->firstPage	= ((Addr_t)region + hdrSzB);
    region->nPages	= npages;
    region->nFree	= npages;
    region->minGen	= MAX_NUM_GENS;
    region->memObj	= memObj;
    region->next	= heap->bigRegions;
    heap->bigRegions	= region;
    heap->numBORegions++;
    for (i = 0;  i < npages;  i++)
	region->objMap[i] = desc;

  /* initialize the descriptor for the region's memory */
    desc->obj		= region->firstPage;
    desc->sizeB		= szB;
    desc->state		= BO_FREE;
    desc->region	= region;

#ifdef BO_DEBUG
SayDebug ("BO_AllocRegion: %d pages @ %#x\n", npages, region->firstPage);
#endif
    return desc;

} /* end of BO_AllocRegion */


/* BO_Alloc:
 *
 * Allocate a big object of the given size.
 */
bigobj_desc_t *BO_Alloc (heap_t *heap, int gen, Addr_t objSzB)
{
    bigobj_desc_t   *hdr, *dp, *newObj;
    bigobj_region_t *region;
    Addr_t	    totSzB;
    int		    i, npages, firstPage;

    totSzB = ROUNDUP(objSzB, BIGOBJ_PAGE_SZB);
    npages = (totSzB >> BIGOBJ_PAGE_SHIFT);

  /* search for a free object that is big enough (first-fit) */
    hdr = heap->freeBigObjs;
    for (dp = hdr->next;  (dp != hdr) && (dp->sizeB < totSzB);  dp = dp->next)
	continue;

    if (dp == hdr) {
      /* no free object fits, so allocate a new region */
	dp = BO_AllocRegion (heap, totSzB);
	region = dp->region;
	if (dp->sizeB == totSzB)
	  /* allocate the whole region to the object */
	    newObj = dp;
	else {
	  /* split the free object */
	    newObj		= NEW_OBJ(bigobj_desc_t);
	    newObj->obj		= dp->obj;
	    newObj->region	= region;
	    dp->obj		= (Addr_t)(dp->obj) + totSzB;
	    dp->sizeB		-= totSzB;
	    ADD_BODESC(heap->freeBigObjs, dp);
	    firstPage		= ADDR_TO_BOPAGE(region, newObj->obj);
	    for (i = 0;  i < npages;  i++)
		region->objMap[firstPage+i] = newObj;
	}
    }
    else if (dp->sizeB == totSzB) {
	REMOVE_BODESC(dp);
	newObj = dp;
	region = dp->region;
    }
    else {
      /* split the free object, leaving dp in the free list. */
	region		= dp->region;
	newObj		= NEW_OBJ(bigobj_desc_t);
	newObj->obj	= dp->obj;
	newObj->region	= region;
	dp->obj		= (Addr_t)(dp->obj) + totSzB;
	dp->sizeB	-= totSzB;
	firstPage	= ADDR_TO_BOPAGE(region, newObj->obj);
	for (i = 0;  i < npages;  i++)
	    dp->region->objMap[firstPage+i] = newObj;
    }

    newObj->sizeB	= objSzB;
    newObj->state	= BO_YOUNG;
    newObj->gen		= gen;
    region->nFree	-= npages;

    if (region->minGen > gen) {
      /* update the generation part of the descriptor */
	region->minGen = gen;
	MarkRegion (BIBOP, (ml_val_t *)region, MEMOBJ_SZB(region->memObj),
	    AID_BIGOBJ(gen));
	BIBOP[BIBOP_ADDR_TO_INDEX(region)] = AID_BIGOBJ_HDR(gen);
    }

#ifdef BO_DEBUG
SayDebug ("BO_Alloc: %d bytes @ %#x\n", objSzB, newObj->obj);
PrintRegionMap(region);
#endif
    return newObj;

} /* end of BO_Alloc */


/* BO_Free:
 *
 * Mark a big object as free and add it to the free list.
 */
void BO_Free (heap_t *heap, bigobj_desc_t *desc)
{
    bigobj_region_t *region = desc->region;
    bigobj_desc_t   *dp;
    int		    firstPage, lastPage, i, j;
    Addr_t	    totSzB = ROUNDUP(desc->sizeB, BIGOBJ_PAGE_SZB);

    firstPage = ADDR_TO_BOPAGE(region, desc->obj);
    lastPage = firstPage + (totSzB >> BIGOBJ_PAGE_SHIFT);

#ifdef BO_DEBUG
SayDebug ("BO_Free: @ %#x, bibop gen = %x, gen = %d, state = %d, pages=[%d..%d)\n",
desc->obj, (unsigned)EXTRACT_GEN(ADDR_TO_PAGEID(BIBOP, desc->obj)), desc->gen, desc->state, firstPage, lastPage);
PrintRegionMap(region);
#endif
    if ((firstPage > 0) && BO_IS_FREE(region->objMap[firstPage-1])) {
      /* coalesce with adjacent free object */
	dp = region->objMap[firstPage-1];
	REMOVE_BODESC(dp);
	for (i = ADDR_TO_BOPAGE(region, dp->obj); i < firstPage;  i++)
	    region->objMap[i] = desc;
	desc->obj = dp->obj;
	totSzB += dp->sizeB;
	FREE (dp);
    }

    if ((lastPage < region->nPages) && BO_IS_FREE(region->objMap[lastPage])) {
      /* coalesce with adjacent free object */
	dp = region->objMap[lastPage];
	REMOVE_BODESC(dp);
	for (i = lastPage, j = i+(dp->sizeB >> BIGOBJ_PAGE_SHIFT); i < j;  i++)
	    region->objMap[i] = desc;
	totSzB += dp->sizeB;
	FREE (dp);
    }

    desc->sizeB = totSzB;
    desc->state = BO_FREE;

    region->nFree += (lastPage - firstPage);
    /** what if (region->nFree == region->nPages) ??? **/

  /* add desc to the free list */
    ADD_BODESC(heap->freeBigObjs, desc);

} /* end of BO_Free */


/* BO_GetDesc:
 *
 * Given an address into a big object, return the object's descriptor.
 */
bigobj_desc_t *BO_GetDesc (ml_val_t addr)
{
    bibop_t	    bibop = BIBOP;
    int		    i;
    aid_t	    aid;
    bigobj_region_t *rp;

    for (i = BIBOP_ADDR_TO_INDEX(addr);  !BO_IS_HDR(aid = bibop[i]);  i--)
	continue;

    rp = (bigobj_region_t *)BIBOP_INDEX_TO_ADDR(i);

    return ADDR_TO_BODESC(rp, addr);

} /* end of BO_GetDesc */


/* BO_AddrToCodeObjTag:
 *
 * Return the tag of the code object containing the given PC (or else
 * NIL).
 */
char *BO_AddrToCodeObjTag (Word_t pc)
{
    bigobj_region_t	*region;
    aid_t		aid;

    aid = ADDR_TO_PAGEID(BIBOP, pc);

    if (IS_BIGOBJ_AID(aid)) {
	int		indx = BIBOP_ADDR_TO_INDEX(pc);
	while (!BO_IS_HDR(aid))
	    aid = BIBOP[--indx];
	region = (bigobj_region_t *)BIBOP_INDEX_TO_ADDR(indx);
	return BO_GetCodeObjTag (ADDR_TO_BODESC(region, pc));
    }
    else
	return NIL(char *);

} /* end of BO_AddrToCodeObjTag */


/* BO_GetCodeObjTag:
 *
 * Return the tag of the given code object.
 */
char *BO_GetCodeObjTag (bigobj_desc_t *bdp)
{
    Byte_t		*lastByte;
    int			kx;

    lastByte = (Byte_t *)(bdp->obj) + bdp->sizeB - 1;
    kx = *lastByte * WORD_SZB;
    return lastByte - kx + 1;
} /* end of BO_GetCodeObjTag */

