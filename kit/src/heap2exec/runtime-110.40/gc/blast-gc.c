/* blast-gc.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * This is the garbage collector for compacting a blasted object.
 *
 * NOTE: the extraction of literals could cause a space overflow.
 */

#include <stdio.h>
#include "ml-base.h"
#include "ml-limits.h"
#include "ml-state.h"
#include "ml-values.h"
#include "memory.h"
#include "card-map.h"
#include "heap.h"
#include "tags.h"
#include "copy-loop.h"
#include "heap-monitor.h"
#include "ml-timer.h"
#include "ml-heap-image.h"
#include "blast-out.h"
#include "addr-hash.h"
#include "c-globals-tbl.h"
#include "ml-objects.h"
#include "ml-globals.h"


PVT bool_t	repairHeap;		/* this is TRUE, as long as it is cheaper */
					/* to repair the heap, than to complete */
					/* the collection */
PVT bool_t	finishGC;		/* this is TRUE, when we are finishing a */
					/* garbage collection after blasting. */
PVT int		maxCollectedGen;	/* the oldest generation being collected */
PVT ml_val_t	*savedTop		/* save to-space top pointers */
		    [MAX_NUM_GENS][NUM_ARENAS];
PVT export_table_t *ExportTbl;		/* the table of exported symbols */
PVT addr_tbl_t	*EmbObjTbl;		/* the table of embedded object references */

/* typedef struct repair repair_t; */  /* in heap.h */
struct repair {
    ml_val_t	*loc;			/* the location to repair */
    ml_val_t	val;			/* the old value */
};

/* record a location in a given arena for repair */
#define NOTE_REPAIR(ap, location, value)	{	\
	arena_t	*__ap = (ap);				\
	if (repairHeap) {				\
	    repair_t	*__rp = __ap->repairList - 1;	\
	    if ((ml_val_t *)__rp > __ap->nextw) {	\
		__rp->loc = (location);			\
		__rp->val = (value);			\
		__ap->repairList = __rp;		\
	    }						\
	    else					\
		repairHeap = FALSE;			\
	}						\
    }

/* local routines */
PVT void BlastGC_RepairHeap (ml_state_t *msp, int maxGen);
PVT void BlastGC_FinishGC (ml_state_t *msp, int maxGen);
PVT void BlastGC_Flip (heap_t *heap, int gen);
PVT status_t BlastGC_SweepToSpace (heap_t *heap, aid_t maxAid);
/*
PVT bool_t BlastGC_SweepToSpArrays (heap_t *heap, arena_t *tosp, card_map_t *cm);
*/
PVT ml_val_t BlastGC_ForwardObj (heap_t *heap, ml_val_t obj, aid_t id);
PVT bigobj_desc_t *BlastGC_ForwardBigObj (
	heap_t *heap, ml_val_t *p, ml_val_t obj, aid_t aid);
PVT embobj_info_t *EmbObjLookup (addr_tbl_t *tbl, Addr_t addr, embobj_kind_t kind);
PVT void BlastGC_AssignLits (Addr_t addr, void *_closure, void *_info);
PVT void BlastGC_ExtractLits (Addr_t addr, void *_closure, void *_info);

struct assignlits_clos {	/* the closure for BlastGC_AssignLits */
    Word_t	id;		  /* the heap image chunk index for */
				  /* embedded literals */
    Word_t	offset;		  /* the offset of the next literal */
};

struct extractlits_clos {	/* the closure for BlastGC_ExtractLits */
    writer_t	*wr;
    Word_t	offset;		  /* the offset of the next literal; this is */
				  /* used to align reals. */
};


/* check to see if we need to extend the number of flipped generations */
#define CHECK_GEN(heap, g)	{		\
	int	__g = (g);			\
	if (__g > maxCollectedGen)		\
	    BlastGC_Flip ((heap), __g);		\
    }

/* BlastGC_CheckWord:
 *
 * Check an ML value for external references, etc.
 */
#define BlastGC_CheckWord(heap, bibop, p, maxAid, errFlg) {			\
	ml_val_t	__w = *(p);						\
/*SayDebug ("CheckWord @ %#x --> %#x: ", p, __w);*/\
	if (isBOXED(__w)) {							\
	    aid_t	__aid = ADDR_TO_PAGEID(bibop, __w);			\
	    if (isUNMAPPED(__aid)) {					\
	      /* an external reference */					\
/*SayDebug ("external reference\n");*/\
		if ((! finishGC) && (ExportCSymbol(ExportTbl, __w) == ML_unit))	\
		    (errFlg) = TRUE;						\
	    }									\
	    else if (IS_BIGOBJ_AID(__aid))					\
/*{SayDebug ("big-object\n");*/\
		BlastGC_ForwardBigObj(heap, p, __w, __aid);			\
/*}*/\
	    else if (IS_FROM_SPACE(__aid, maxAid))				\
/*{SayDebug ("regular object\n");*/\
		*(p) = BlastGC_ForwardObj(heap, __w, __aid);			\
/*}*/\
	}									\
/*else SayDebug ("unboxed \n");*/\
    }


/* BlastGC:
 *
 */
blast_res_t BlastGC (ml_state_t *msp, ml_val_t *root, int gen)
{
    heap_t	*heap = msp->ml_heap;
    bibop_t	bibop = BIBOP;
    blast_res_t	result;
    bool_t	errFlg = FALSE;

  /* Allocates the export and embedded object tables */
    ExportTbl = NewExportTbl();
    EmbObjTbl = MakeAddrTbl(LOG_BYTES_PER_WORD, 64);

    result.exportTbl	= ExportTbl;
    result.embobjTbl	= EmbObjTbl;

  /* Initialize, by flipping the generations upto the one including the object */
    repairHeap = TRUE;
    finishGC = FALSE;
    maxCollectedGen = 0;
    BlastGC_Flip (heap, gen);

  /* Scan the object root */
    BlastGC_CheckWord (heap, bibop, root, AID_MAX, errFlg);
    if (errFlg) {
	result.error = TRUE;
	return result;
    }

  /* Sweep to-space */
    if (BlastGC_SweepToSpace(heap, AID_MAX) == FAILURE) {
	result.error = TRUE;
	return result;
    }

    result.error	= FALSE;
    result.needsRepair	= repairHeap;
    result.maxGen	= maxCollectedGen;

    return result;

} /* end of BlastGC. */


/* BlastGC_AssignLitAddrs:
 *
 * Assign relocation addresses to the embedded literals that are going to be
 * extracted.  The arguments to this are the blast result (containing the
 * embedded literal table), the ID of the heap image chunk that the string
 * literals are to be stored in, and the starting offset in that chunk.
 * This returns the address immediately following the last embedded literal.
 *
 * NOTE: this code will break if the size of the string space, plus embedded
 * literals exceeds 16Mb.
 */
Addr_t BlastGC_AssignLitAddrs (blast_res_t *res, int id, Addr_t offset)
{
    struct assignlits_clos closure;

    closure.offset = offset;
    closure.id = id;
    AddrTblApply (EmbObjTbl, &closure, BlastGC_AssignLits);

    return closure.offset;

} /* end of BlastGC_AssignLitAddrs */


/* BlastGC_BlastLits:
 *
 * Blast out the embedded literals.
 */
void BlastGC_BlastLits (writer_t *wr)
{
    struct extractlits_clos closure;

    closure.wr = wr;
    closure.offset = 0;
    AddrTblApply (EmbObjTbl, &closure, BlastGC_ExtractLits);

} /* end of BlastGC_BlastLits */


/* BlastGC_FinishUp:
 *
 * Finish up the blast-out operation.  This means either repairing the heap,
 * or completing the GC.
 */
void BlastGC_FinishUp (ml_state_t *msp, blast_res_t *res)
{
    if (res->needsRepair)
	BlastGC_RepairHeap (msp, res->maxGen);
    else
	BlastGC_FinishGC (msp, res->maxGen);

    FreeExportTbl (ExportTbl);
    FreeAddrTbl (EmbObjTbl, TRUE);

} /* BlastGC_FinishUp */

/* BlastGC_RepairHeap:
 */
PVT void BlastGC_RepairHeap (ml_state_t *msp, int maxGen)
{
    heap_t	*heap = msp->ml_heap;
    int		i, j;

#ifdef VERBOSE
SayDebug ("Repairing blast GC (maxGen = %d of %d)\n", maxGen, heap->numGens);
#endif
    for (i = 0;  i < maxGen;  i++) {
	gen_t		*gen = heap->gen[i];

#define REPAIR(INDX)	{						\
	arena_t		*__ap = gen->arena[INDX];			\
	if (isACTIVE(__ap)) {						\
	    repair_t	*__stop, *__rp;					\
	    __stop = (repair_t *)(__ap->tospTop);			\
	    for (__rp = __ap->repairList;  __rp < __stop;  __rp++) {	\
		ml_val_t	*__p = __rp->loc;			\
		if (INDX != PAIR_INDX)					\
		    __p[-1] = FOLLOW_FWDOBJ(__p)[-1];			\
		__p[0] = __rp->val;					\
	    }								\
	}								\
    } /* end of REPAIR */

      /* repair the arenas */
	REPAIR(RECORD_INDX);
	REPAIR(PAIR_INDX);
	REPAIR(STRING_INDX);
	REPAIR(ARRAY_INDX);

      /* free the to-space object, and reset the BIBOP marks */
	for (j = 0;  j < NUM_ARENAS;  j++) {
	    arena_t	*ap = gen->arena[j];
	    if (isACTIVE(ap)) {
	      /* un-flip the spaces; note that FreeGeneration needs the from-space
	       * information.
	       */
		ml_val_t	*tmpBase = ap->tospBase;
		Addr_t		tmpSizeB = ap->tospSizeB;
		ml_val_t	*tmpTop = ap->tospTop;
		ap->nextw	=
		ap->sweep_nextw = ap->frspTop;
		ap->tospBase	= ap->frspBase;
		ap->frspBase	= tmpBase;
		ap->tospSizeB	= ap->frspSizeB;
		ap->frspSizeB	= tmpSizeB;
		ap->tospTop	= savedTop[i][j];
		ap->frspTop	= tmpTop;
	    }
	} /* end of for */
      /* free the to-space memory object */
	{
	    mem_obj_t	*tmpObj = gen->fromObj;
	    gen->fromObj = gen->toObj;
	    gen->toObj = tmpObj;
	    FreeGeneration (heap, i);
	}
    } /* end of for */

} /* end of BlastGC_RepairHeap */


/* BlastGC_FinishGC:
 *
 * Complete the partial garbage collection.
 */
PVT void BlastGC_FinishGC (ml_state_t *msp, int maxGen)
{
    heap_t	*heap = msp->ml_heap;
    bibop_t	bibop = BIBOP;
    bool_t	dummy = FALSE;
    int		i, j;
    aid_t	maxAid;

#ifdef VERBOSE
SayDebug ("Completing blast GC (maxGen = %d of %d)\n", maxGen, heap->numGens);
#endif
    finishGC = TRUE;
    maxAid = MAKE_MAX_AID(maxGen);

  /* allocate new dirty vectors for the flipped generations */
    for (i = 0;  i < maxGen;  i++) {
	gen_t	*gen = heap->gen[i];
	if (isACTIVE(gen->arena[ARRAY_INDX]))
	    NewDirtyVector(gen);
    }

  /* collect the roots */
#define CheckRoot(p)	{					\
	ml_val_t	*__p = (p);				\
	BlastGC_CheckWord (heap, bibop, __p, maxAid, dummy);	\
    }

    for (i = 0;  i < NumCRoots;  i++)
	CheckRoot(CRoots[i]);

    CheckRoot(&(msp->ml_arg));
    CheckRoot(&(msp->ml_cont));
    CheckRoot(&(msp->ml_closure));
    CheckRoot(&(msp->ml_linkReg));
    CheckRoot(&(msp->ml_pc));
    CheckRoot(&(msp->ml_exnCont));
    CheckRoot(&(msp->ml_varReg));
    CheckRoot(&(msp->ml_calleeSave[0]));
    CheckRoot(&(msp->ml_calleeSave[1]));
    CheckRoot(&(msp->ml_calleeSave[2]));

  /* sweep the dirty pages of generations over maxGen */
    for (i = maxGen; i < heap->numGens;  i++) {
	gen_t	*gen = heap->gen[i];
	if (isACTIVE(gen->arena[ARRAY_INDX])) {
	    card_map_t	*cm = gen->dirty;
	    if (cm != NIL(card_map_t *)) {
		ml_val_t	*maxSweep = gen->arena[ARRAY_INDX]->sweep_nextw;
		int		card;
#if (!defined(BIT_CARDS) && defined(TOSPACE_ID))
		FOR_DIRTY_CARD (cm, maxGen, card, {
		    ml_val_t	*p = (cm->baseAddr + (card*CARD_SZW));
		    ml_val_t	*q = p + CARD_SZW;
		    int		mark = i+1;
		    if (q > maxSweep)
		      /* don't sweep above the allocation high-water mark */
			q = maxSweep;
		    for (;  p < q;  p++) {
			ml_val_t	w = *p;
			if (isBOXED(w)) {
			    aid_t	aid = ADDR_TO_PAGEID(bibop, w);
			    int		targetGen;
			    if (IS_FROM_SPACE(aid, maxAid)) {
			      /* this is a from-space object */
			        if (IS_BIGOBJ_AID(aid)) {
				    bigobj_desc_t	*dp;
				    dp = BlastGC_ForwardBigObj (heap, p, w, aid);
				    targetGen = dp->gen;
			        }
			        else {
				    *p =
				    w = BlastGC_ForwardObj(heap, w, aid);
				    aid = ADDR_TO_PAGEID(bibop, w);
				    if (IS_TOSPACE_AID(aid))
				        targetGen = TOSPACE_GEN(aid);
				    else
				        targetGen = EXTRACT_GEN(aid);
			        }
			        if (targetGen < mark)
				    mark = targetGen;
			    }
		        }
		    } /* end of for */
		  /* re-mark the card */
		    ASSERT(cm->map[card] <= mark);
		    if (mark <= i)
			cm->map[card] = mark;
		    else if (i == maxGen)
			cm->map[card] = CARD_CLEAN;
		});
#elif (!defined(BIT_CARDS))
		FOR_DIRTY_CARD (cm, maxGen, card, {
		    ml_val_t	*p = (cm->baseAddr + (card*CARD_SZW));
		    ml_val_t	*q = p + CARD_SZW;
		    int		mark = i+1;
		    if (q > maxSweep)
		      /* don't sweep above the allocation high-water mark */
			q = maxSweep;
		    for (;  p < q;  p++) {
			ml_val_t	w = *p;
			if (isBOXED(w)) {
			    aid_t	aid = ADDR_TO_PAGEID(bibop, w);
			    int		targetGen;
			    if (IS_FROM_SPACE(aid, maxAid)) {
			      /* this is a from-space object */
			        if (IS_BIGOBJ_AID(aid)) {
				    bigobj_desc_t	*dp;
				    dp = BlastGC_ForwardBigObj (heap, p, w, aid);
				    targetGen = dp->gen;
			        }
			        else {
				    *p =
				    w = BlastGC_ForwardObj(heap, w, aid);
				    targetGen = EXTRACT_GEN(ADDR_TO_PAGEID(bibop, w));
			        }
			        if (targetGen < mark)
				    mark = targetGen;
			    }
		        }
		    } /* end of for */
		  /* re-mark the card */
		    ASSERT(cm->map[card] <= mark);
		    if (mark <= i)
			cm->map[card] = mark;
		    else if (i == maxGen)
			cm->map[card] = CARD_CLEAN;
		});
#else
  /* BIT_CARDS */
#endif
	    }
	}
    }

  /* sweep to-space */
    BlastGC_SweepToSpace (heap, maxAid);

  /* Scan the array spaces of the flipped generations, marking dirty pages */
    for (i = 1;  i < maxGen;  i++) {
	gen_t		*gen = heap->gen[i];
	arena_t		*ap = gen->arena[ARRAY_INDX];
	if (isACTIVE(ap)) {
	    card_map_t	*cm = gen->dirty;
	    int		card;
	    ml_val_t	*p, *stop, w;

	    p = ap->tospBase;
	    card = 0;
	    while (p < ap->nextw) {
		int	mark = i+1;
		stop = (ml_val_t *)(((Addr_t)p + CARD_SZB) & ~(CARD_SZB - 1));
		if (stop > ap->nextw)
		    stop = ap->nextw;
		while (p < stop) {
		    if (isBOXED(w = *p++)) {
			aid_t	aid = ADDR_TO_PAGEID(bibop, w);
			int	targetGen;

			if (IS_BIGOBJ_AID(aid)) {
			    bigobj_desc_t	*dp = BO_GetDesc(w);
			    targetGen = dp->gen;
			}
			else
			    targetGen = EXTRACT_GEN(aid);
			if (targetGen < mark) {
			    mark = targetGen;
			    if (mark == 1) {
				p = stop;
				break;  /* nothing dirtier than 1st generation */
			    }
			}
		    }
		}
		if (mark <= i)
		    cm->map[card] = mark;
		else
		    cm->map[card] = CARD_CLEAN;
		card++;
	    }
	}
    }

  /* reclaim space */
    for (i = 0;  i < maxGen;  i++) {
	FreeGeneration (heap, i);
#ifdef TOSPACE_ID
	for (j = 0;  j < NUM_ARENAS;  j++) {
	    arena_t		*ap = heap->gen[i]->arena[j];
	    if (isACTIVE(ap))
		MarkRegion (bibop, ap->tospBase, ap->tospSizeB, ap->id);
	}
#endif
    }

  /* remember the top of to-space in the collected generations */
    for (i = 0;  i < maxGen;  i++) {
	gen_t *g = heap->gen[i];
	if (i == heap->numGens-1) {
	  /* the oldest generation has only "young" objects */
	    for (j = 0;  j < NUM_ARENAS;  j++) {
		if (isACTIVE(g->arena[j]))
		    g->arena[j]->oldTop = g->arena[j]->tospBase;
		else
		    g->arena[j]->oldTop = NIL(ml_val_t *);
	    }
	}
	else {
	    for (j = 0;  j < NUM_ARENAS;  j++) {
		if (isACTIVE(g->arena[j]))
		    g->arena[j]->oldTop = g->arena[j]->nextw;
		else
		    g->arena[j]->oldTop = NIL(ml_val_t *);
	    }
	}
    }

    HeapMon_UpdateHeap (heap, maxSweptGen);

#ifdef GC_STATS
  /* Count the number of forwarded bytes */
    for (i = 0;  i < maxGen;  i++) {
	for (j = 0;  j < NUM_ARENAS;  j++) {
	    arena_t	*ap = heap->gen[i]->arena[j];
	    if (isACTIVE(ap)) {
		CNTR_INCR(&(heap->numCopied[i][j]), ap->nextw - ap->tospBase);
	    }
	}
    }
#endif

} /* end of BlastGC_FinishGC */


/* BlastGC_Flip:
 *
 * Flip additional generations from maxCollectedGen+1 .. gen.  We allocate
 * a to-space that is the same size as the existing from-space.
 */
PVT void BlastGC_Flip (heap_t *heap, int gen)
{
    int		i, j;
    Addr_t	newSz;

    for (i = maxCollectedGen;  i < gen;  i++) {
	gen_t	*g = heap->gen[i];
	for (j = 0;  j < NUM_ARENAS;  j++) {
	    arena_t	*ap = g->arena[j];
	    if (isACTIVE(ap)) {
		ASSERT ((j == STRING_INDX) || (ap->nextw == ap->sweep_nextw));
	        savedTop[i][j] = ap->tospTop;
		FLIP_ARENA(ap);
		newSz = (Addr_t)(ap->frspTop) - (Addr_t)(ap->frspBase);
		if (i == 0)
		  /* need to guarantee space for future minor collections */
		    newSz += heap->allocSzB;
		if (j == PAIR_INDX)
		    newSz += 2*WORD_SZB;
		ap->tospSizeB = RND_MEMOBJ_SZB(newSz);
	    }
	}
	g->fromObj = g->toObj;
#ifdef VERBOSE
SayDebug ("New Generation %d:\n", i+1);
#endif
	if (NewGeneration(g) == FAILURE)
	    Die ("unable to allocate to-space for generation %d\n", i+1);
     /* initialize the repair lists */
	for (j = 0;  j < NUM_ARENAS;  j++) {
	    arena_t	*ap = g->arena[j];
#ifdef VERBOSE
if (isACTIVE(ap)) SayDebug ("  %#x:  [%#x, %#x)\n", ap->id, ap->tospBase, ap->tospTop);
#endif
	    if (isACTIVE(ap))
		ap->repairList = (repair_t *)(ap->tospTop);
	}
    }

    maxCollectedGen = gen;

} /* end of BlastGC_Flip */

/* BlastGC_SweepToSpace:
 * Sweep the to-space arenas.  Because there are few references forward in time, we
 * try to completely scavenge a younger generation before moving on to the
 * next oldest.
 */
PVT status_t BlastGC_SweepToSpace (heap_t *heap, aid_t maxAid)
{
    int		i;
    bool_t	swept;
    bibop_t	bibop = BIBOP;
    bool_t	errFlg = FALSE;

#define SweepToSpArena(gen, indx)	{					\
	arena_t	    *__ap = (gen)->arena[(indx)];				\
	if (isACTIVE(__ap)) {							\
	    ml_val_t    *__p, *__q;						\
	    __p = __ap->sweep_nextw;						\
	    if (__p < __ap->nextw) {						\
		swept = TRUE;							\
		do {								\
		    for (__q = __ap->nextw;  __p < __q;  __p++) {		\
			BlastGC_CheckWord(heap, bibop, __p, maxAid, errFlg);	\
		    }								\
		} while (__q != __ap->nextw);					\
		__ap->sweep_nextw = __q;					\
	    }									\
	}									\
    } /* SweepToSpArena */

    do {
	swept = FALSE;
	for (i = 0;  i < maxCollectedGen;  i++) {
	    gen_t	*gen = heap->gen[i];

	  /* Sweep the record and pair arenas */
	    SweepToSpArena(gen, RECORD_INDX);
	    SweepToSpArena(gen, PAIR_INDX);
	    SweepToSpArena(gen, ARRAY_INDX);
	}
    } while (swept && (!errFlg));

    return (errFlg ? FAILURE : SUCCESS);

} /* end of SweepToSpace */


/* BlastGC_ForwardObj:
 *
 * Forward an object.
 */
PVT ml_val_t BlastGC_ForwardObj (heap_t *heap, ml_val_t v, aid_t id)
{
    ml_val_t	*obj = PTR_MLtoC(ml_val_t, v);
    int		gen = EXTRACT_GEN(id);
    ml_val_t	*new_obj;
    ml_val_t	desc;
    Word_t	len;
    arena_t	*arena;

    if (! finishGC)
	CHECK_GEN(heap, gen);

    switch (EXTRACT_OBJC(id)) {
      case OBJC_record: {
	desc = obj[-1];
	switch (GET_TAG(desc)) {
	  case DTAG_vec_hdr:
	  case DTAG_arr_hdr:
	    len = 2;
	    break;
	  case DTAG_forward:
	  /* This object has already been forwarded */
	    return PTR_CtoML(FOLLOW_FWDOBJ(obj));
	  default:
	    len = GET_LEN(desc);
	}
	arena = heap->gen[gen-1]->arena[RECORD_INDX];
      } break;

      case OBJC_pair: {
	ml_val_t	w;

	w = obj[0];
	if (isDESC(w))
	    return PTR_CtoML(FOLLOW_FWDPAIR(w, obj));
	else {
	  /* forward the pair */
	    arena = heap->gen[gen-1]->arena[PAIR_INDX];
	    new_obj = arena->nextw;
	    arena->nextw += 2;
	    new_obj[0] = w;
	    new_obj[1] = obj[1];
	  /* setup the forward pointer in the old pair */
	    NOTE_REPAIR(arena, obj, w);
	    obj[0] =  MAKE_PAIR_FP(new_obj);
	    return PTR_CtoML(new_obj);
	}
      } break;

      case OBJC_string: {
	arena = heap->gen[gen-1]->arena[STRING_INDX];
	desc = obj[-1];
	switch (GET_TAG(desc)) {
	  case DTAG_forward:
	    return PTR_CtoML(FOLLOW_FWDOBJ(obj));
	  case DTAG_raw32:
	    len = GET_LEN(desc);
	    break;
	  case DTAG_raw64:
	    len = GET_LEN(desc);
#ifdef ALIGN_REALDS
#  ifdef CHECK_HEAP
	    if (((Addr_t)arena->nextw & WORD_SZB) == 0) {
		*(arena->nextw) = (ml_val_t)0;
		arena->nextw++;
	    }
#  else
	    arena->nextw = (ml_val_t *)(((Addr_t)arena->nextw) | WORD_SZB);
#  endif
#endif
	    break;
	  default:
	    Die ("bad string tag %d, obj = %#x, desc = %#x",
		GET_TAG(desc), obj, desc);
	}
      } break;

      case OBJC_array: {
	desc = obj[-1];
	switch (GET_TAG(desc)) {
	  case DTAG_forward:
	  /* This object has already been forwarded */
	    return PTR_CtoML(FOLLOW_FWDOBJ(obj));
	  case DTAG_arr_data:
	    len = GET_LEN(desc);
	    break;
	  case DTAG_special:
	  /* we are conservative here, and never nullify special objects */
	    len = 1;
	    break;
	  default:
	    Die ("bad array tag %d, obj = %#x, desc = %#x",
		GET_TAG(desc), obj, desc);
	} /* end of switch */
	arena = heap->gen[gen-1]->arena[ARRAY_INDX];
      } break;

      case OBJC_bigobj:
      default:
	Die("BlastGC_ForwardObj: unknown object class %d @ %#x",
	    EXTRACT_OBJC(id), obj);
    } /* end of switch */

  /* Allocate and initialize a to-space copy of the object */
    new_obj = arena->nextw;
    arena->nextw += (len + 1);
    *new_obj++ = desc;
    COPYLOOP(obj, new_obj, len);

  /* set up the forward pointer, and return the new object. */
    NOTE_REPAIR(arena, obj, *obj);
    obj[-1] = DESC_forwarded;
    obj[0] = (ml_val_t)(Addr_t)new_obj;
    return PTR_CtoML(new_obj);

} /* end of BlastGC_ForwardObj */


/* BlastGC_ForwardBigObj:
 *
 * Forward a big-object obj, where id is the BIBOP entry for obj, and return
 * the big-object descriptor.
 * NOTE: we do not ``promote'' big-objects here, because are not reclaimed
 * when completing th collection.
 */
PVT bigobj_desc_t *BlastGC_ForwardBigObj (
    heap_t	    *heap,
    ml_val_t	    *p,
    ml_val_t	    obj,
    aid_t	    aid)
{
    int		    i;
    bigobj_region_t *region;
    bigobj_desc_t   *dp;
    embobj_info_t   *codeInfo;

    for (i = BIBOP_ADDR_TO_INDEX(obj);  !BO_IS_HDR(aid);  aid = BIBOP[--i])
	continue;
    region = (bigobj_region_t *)BIBOP_INDEX_TO_ADDR(i);
    dp = ADDR_TO_BODESC(region, obj);

    if (! finishGC) {
	CHECK_GEN(heap, dp->gen);
	codeInfo = EmbObjLookup (EmbObjTbl, dp->obj, UNUSED_CODE);
	codeInfo->kind = USED_CODE;
    }

    return dp;

} /* end of BlastGC_ForwardBigObj */


/* EmbObjLookup:
 */
PVT embobj_info_t *EmbObjLookup (addr_tbl_t *tbl, Addr_t addr, embobj_kind_t kind)
{
    embobj_info_t	*p = FindEmbObj(tbl, addr);

    if (p == NIL(embobj_info_t *)) {
	p		= NEW_OBJ(embobj_info_t);
	p->kind		= kind;
	p->codeObj	= NIL(embobj_info_t *);
	AddrTblInsert(tbl, addr, p);
    }

    ASSERT(kind == p->kind);

    return p;

} /* end of EmbObjLookup */

/* BlastGC_AssignLits:
 *
 * Calculate the location of the extracted literal strings in the blasted
 * image, and record their addresses.  This function is passed as an argument
 * to AddrTblApply; its second argument is its "closure," and its third
 * argument is the embedded object info.
 */
PVT void BlastGC_AssignLits (Addr_t addr, void *_closure, void *_info)
{
#ifdef XXX
    struct assignlits_clos *closure = (struct assignlits_clos *) _closure;
    embobj_info_t	*info = (embobj_info_t *) _info;
    int			objSzB;

    switch (info->kind) {
      case UNUSED_CODE:
      case USED_CODE:
	info->relAddr = (ml_val_t)0;
	return;
      case EMB_STRING: {
	    int		nChars = OBJ_LEN(PTR_CtoML(addr));
	    int		nWords = BYTES_TO_WORDS(nChars);
	    if ((nChars != 0) && ((nChars & 0x3) == 0))
	        nWords++;
	    objSzB = nWords * WORD_SZB;
	} break;
      case EMB_REALD:
	objSzB = OBJ_LEN(PTR_CtoML(addr)) * REALD_SZB;
#ifdef ALIGN_REALDS
	closure->offset |= WORD_SZB;
#endif
	break;
    }

    if (info->codeObj->kind == USED_CODE) {
      /* the containing code object is also being exported */
	info->relAddr = (ml_val_t)0;
	return;
    }

    if (objSzB == 0) {
	info->relAddr = ExportCSymbol (ExportTbl,
		(info->kind == EMB_STRING) ? ML_string0 : ML_realarray0);
    }
    else {
      /* assign a relocation address to the object, and bump the offset counter */
	closure->offset += WORD_SZB;  /* space for the descriptor */
	info->relAddr = HIO_TAG_PTR(closure->id, closure->offset);
	closure->offset += objSzB;
    }
#else
Die ("BlastGC_AssignLits");
#endif
} /* end of BlastGC_AssignLits */

/* BlastGC_ExtractLits:
 *
 * Extract the embedded literals that are in otherwise unreferenced code
 * blocks.  This function is passed as an argument to AddrTblApply; its
 * second argument is its "closure," and its third argument is the
 * embedded object info.
 */
PVT void BlastGC_ExtractLits (Addr_t addr, void *_closure, void *_info)
{
    struct extractlits_clos *closure = (struct extractlits_clos *) _closure;
    embobj_info_t	*info = (embobj_info_t *) _info;
    int			objSzB;

    if (info->relAddr == (ml_val_t)0)
	return;

    switch (info->kind) {
      case EMB_STRING: {
	    int		nChars = OBJ_LEN(PTR_CtoML(addr));
	    int		nWords = BYTES_TO_WORDS(nChars);
	    if ((nChars != 0) && ((nChars & 0x3) == 0))
	        nWords++;
	    objSzB = nWords * WORD_SZB;
	} break;
      case EMB_REALD:
	objSzB = OBJ_LEN(PTR_CtoML(addr)) * REALD_SZB;
#ifdef ALIGN_REALDS
	if ((closure->offset & (REALD_SZB-1)) == 0) {
	    /* the descriptor would be 8-byte aligned, which means that the
	     * real number would not be, so add some padding.
	     */
	    WR_Put(closure->wr, 0);
	    closure->offset += 4;
	}
#endif
	break;
    }

    if (objSzB != 0) {
      /* extract the object into the blast buffer (including the descriptor) */
	WR_Write(closure->wr, (void *)(addr - WORD_SZB), objSzB + WORD_SZB);
	closure->offset += (objSzB + WORD_SZB);
    }

} /* end of BlastGC_ExtractLits */
