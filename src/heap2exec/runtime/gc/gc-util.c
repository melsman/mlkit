/* gc-util.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * Garbage collection utility routines.
 *
 */

#include <stdarg.h>
#include "ml-base.h"
#include "ml-limits.h"
#include "ml-values.h"
#include "memory.h"
#include "card-map.h"
#include "heap.h"
#include "heap-monitor.h"


/* NewGeneration:
 *
 * Allocate and partition the space for a generation.
 */
status_t NewGeneration (gen_t *gen)
{
    int		i;
    Addr_t	tot_sz;
    ml_val_t	*p;
    mem_obj_t	*memobj;
    arena_t	*ap;

  /* Compute the total size */
    for (tot_sz = 0, i = 0;  i < NUM_ARENAS;  i++) {
	if (isACTIVE(gen->arena[i]))
	    tot_sz += gen->arena[i]->tospSizeB;
    }

    if ((gen->cacheObj != NIL(mem_obj_t *)) && (MEMOBJ_SZB(gen->cacheObj) >= tot_sz)) {
	memobj = gen->cacheObj;
	gen->cacheObj =  NIL(mem_obj_t *);
    }
    else if ((memobj = MEM_AllocMemObj(tot_sz)) == NIL(mem_obj_t *)) {
	/** Eventually we should try to allocate the generation as separate
	 ** chunks instead of failing.
	 **/
	return FAILURE;
    }

  /* Initialize the chunks */
    gen->toObj = memobj;
#ifdef VERBOSE
SayDebug ("NewGeneration[%d]: tot_sz = %d, [%#x, %#x)\n",
gen->genNum, tot_sz, MEMOBJ_BASE(memobj), MEMOBJ_BASE(memobj) + MEMOBJ_SZB(memobj));
#endif
    for (p = (ml_val_t *)MEMOBJ_BASE(memobj), i = 0;  i < NUM_ARENAS;  i++) {
	ap = gen->arena[i];
	if (isACTIVE(ap)) {
	    ap->tospBase	= p;
	    ap->nextw		= p;
	    ap->sweep_nextw	= p;
	    p = (ml_val_t *)((Addr_t)p + ap->tospSizeB);
	    ap->tospTop	= p;
	    MarkRegion (BIBOP, ap->tospBase, ap->tospSizeB, ap->id);
	    HeapMon_MarkRegion (gen->heap, ap->tospBase, ap->tospSizeB, ap->id);
#ifdef VERBOSE
SayDebug ("  %#x:  [%#x, %#x)\n", ap->id, ap->nextw, p);
#endif
	}
	else {
	    ap->tospBase	= NIL(ml_val_t *);
	    ap->nextw		= NIL(ml_val_t *);
	    ap->sweep_nextw	= NIL(ml_val_t *);
	    ap->tospTop		= NIL(ml_val_t *);
	}
    }

    ap = gen->arena[PAIR_INDX];
    if (isACTIVE(ap)) {
      /* The first slot of pair-space cannot be used, so that poly-equal won't fault */
	*(ap->nextw++) = ML_unit;
	*(ap->nextw++) = ML_unit;
	ap->tospBase = ap->nextw;
	ap->tospSizeB -= (2*WORD_SZB);
	ap->sweep_nextw = ap->nextw;
    }   

    return SUCCESS;

} /* end of NewGeneration */


/* FreeGeneration:
 */
void FreeGeneration (heap_t *heap, int g)
{
    gen_t	*gen = heap->gen[g];
    int		i;

    if (gen->fromObj == NIL(mem_obj_t *))
	return;

#ifdef VERBOSE
SayDebug ("FreeGeneration [%d]: [%#x, %#x)\n", g+1, MEMOBJ_BASE(gen->fromObj),
MEMOBJ_BASE(gen->fromObj) + MEMOBJ_SZB(gen->fromObj));
#endif
    if (g < heap->cacheGen) {
	if (gen->cacheObj != NIL(mem_obj_t *)) {
	    if (MEMOBJ_SZB(gen->cacheObj) > MEMOBJ_SZB(gen->fromObj))
		MEM_FreeMemObj (gen->fromObj);
	    else {
		MEM_FreeMemObj (gen->cacheObj);
		gen->cacheObj = gen->fromObj;
	    }
	}
	else
	    gen->cacheObj = gen->fromObj;
    }
    else
	MEM_FreeMemObj (gen->fromObj);

/** NOTE: since the arenas are contiguous, we could do this in one call **/
    gen->fromObj = NIL(mem_obj_t *);
    for (i = 0;  i < NUM_ARENAS;  i++) {
	arena_t		*ap = gen->arena[i];
	if (ap->frspBase != NIL(ml_val_t *)) {
	    MarkRegion (BIBOP, ap->frspBase, ap->frspSizeB, AID_UNMAPPED);
	    HeapMon_MarkRegion (heap, ap->frspBase, ap->frspSizeB, AID_UNMAPPED);
	    ap->frspBase = NIL(ml_val_t *);
	    ap->frspSizeB = 0;
	    ap->frspTop = NIL(ml_val_t *);
	}
    }

} /* end of FreeGeneration */


/* NewDirtyVector:
 * Bind in a new dirty vector for the given generation, reclaiming the old
 * vector.
 */
void NewDirtyVector (gen_t *gen)
{
    arena_t	*ap = gen->arena[ARRAY_INDX];
    int		vecSz = (ap->tospSizeB / CARD_SZB);
    int		allocSzB = CARD_MAP_SZ(vecSz);

    if (gen->dirty == NIL(card_map_t *)) {
	gen->dirty = (card_map_t *)MALLOC(allocSzB);
	gen->dirty->mapSzB = allocSzB;
    }
    else if (allocSzB > gen->dirty->mapSzB) {
	FREE(gen->dirty);
	gen->dirty = (card_map_t *)MALLOC(allocSzB);
	gen->dirty->mapSzB = allocSzB;
    }
    if (gen->dirty == NIL(card_map_t *)) {
	Die ("unable to malloc dirty vector");
    }
    gen->dirty->baseAddr = ap->tospBase;
    gen->dirty->numCards = vecSz;
#ifndef BIT_CARDS
    memset (gen->dirty->map, CARD_CLEAN, allocSzB - (sizeof(card_map_t) - WORD_SZB));
#else
    memset (gen->dirty->map, 0, allocSzB - (sizeof(card_map_t) - WORD_SZB));
#endif

} /* end of NewDirtyVector. */


/* MarkRegion:
 *
 * Mark the BIBOP entries corresponding to the range [baseAddr, baseAddr+szB)
 * with aid.
 */
void MarkRegion (bibop_t bibop, ml_val_t *baseAddr, Word_t szB, aid_t aid)
{
#ifdef TWO_LEVEL_MAP
#  error two level map not supported
#else
    int		start = BIBOP_ADDR_TO_INDEX(baseAddr);
    int		end = BIBOP_ADDR_TO_INDEX(((Addr_t)baseAddr)+szB);
#ifdef VERBOSE
/*SayDebug("MarkRegion [%#x..%#x) as %#x\n", baseAddr, ((Addr_t)baseAddr)+szB, aid); */
#endif

    while (start < end) {
	bibop[start++] = aid;
    }
#endif

} /* end of MarkRegion */


/* ScanWeakPtrs:
 *
 * Scan the list of weak pointers, nullifying those that refer to dead
 * (i.e., from-space) objects.
 */
void ScanWeakPtrs (heap_t *heap)
{
    ml_val_t	*p, *q, *obj, *obj_start, desc, fwdObj;

/* SayDebug ("ScanWeakPtrs:\n"); */
    for (p = heap->weakList;  p != NIL(ml_val_t *);  p = q) {
	q = PTR_MLtoC(ml_val_t, UNMARK_PTR(p[0]));
	obj = (ml_val_t *)(Addr_t)UNMARK_PTR(p[1]);
/* SayDebug ("  %#x --> %#x ", p+1, obj); */

	switch (EXTRACT_OBJC(ADDR_TO_PAGEID(BIBOP, obj))) {
	  case OBJC_new:
	  case OBJC_record:
	  case OBJC_string:
	  case OBJC_array:
	    for (obj_start = obj;  !isDESC(desc = obj_start[-1]);  obj_start--)
		continue;
	    if (desc == DESC_forwarded) {
		p[0] = DESC_weak;
		p[1] = PTR_CtoML(FOLLOW_FWDOBJ(obj_start, obj));
/* SayDebug ("forwarded to %#x\n", FOLLOW_FWDOBJ(obj_start, obj)); */
	    }
	    else {
		p[0] = DESC_null_weak;
		p[1] = ML_unit;
/* SayDebug ("nullified\n"); */
	    }
	    break;
	  case OBJC_pair:
	    obj_start = (ml_val_t *)((Addr_t)obj & ~(PAIR_SZB-1));
	    if (isDESC(desc = obj_start[0])) {
		p[0] = DESC_weak;
		p[1] = PTR_CtoML(FOLLOW_FWDPAIR(desc, obj_start, obj));
/* SayDebug ("(pair) forwarded to %#x\n", FOLLOW_FWDPAIR(desc, obj_start, obj)); */
	    }
	    else {
		p[0] = DESC_null_weak;
		p[1] = ML_unit;
/* SayDebug ("(pair) nullified\n"); */
	    }
	    break;
	  case OBJC_bigobj:
	    Die ("weak big object");
	    break;
	} /* end of switch */
    }

    heap->weakList = NIL(ml_val_t *);

} /* end of ScanWeakPtrs */

