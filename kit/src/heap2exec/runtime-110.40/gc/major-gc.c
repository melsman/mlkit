/* major-gc.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * This is the regular garbage collector (for collecting the
 * generations).
 */

#ifdef PAUSE_STATS		/* GC pause statistics are UNIX dependent */
#  include "ml-unixdep.h"
#endif

#include "ml-base.h"
#include "ml-limits.h"
#include "ml-state.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "memory.h"
#include "card-map.h"
#include "heap.h"
#include "tags.h"
#include "copy-loop.h"
#include "heap-monitor.h"
#include "ml-timer.h"
#include "gc-stats.h"

#ifdef GC_STATS
long		lastMinorGC = 0;
long		numUpdates = 0;
long		numBytesAlloc = 0;
long		numBytesCopied = 0;
#endif

#ifdef BO_REF_STATS
PVT long numBO1, numBO2, numBO3;
#define IFBO_COUNT1(aid)	{if (IS_BIGOBJ_AID(aid)) numBO1++;}
#define BO2_COUNT		(numBO2)++
#define BO3_COUNT		(numBO3)++
#else
#define IFBO_COUNT1(aid)	{}
#define BO2_COUNT		{}
#define BO3_COUNT		{}
#endif

#ifdef COUNT_CARDS
#ifndef BIT_CARDS
PVT unsigned long cardCnt1[MAX_NUM_GENS], cardCnt2[MAX_NUM_GENS];
#define COUNT_CARD1(i)	(cardCnt1[i]++)
#define COUNT_CARD2(i)	(cardCnt2[i]++)
#else
PVT unsigned long cardCnt[MAX_NUM_GENS];
#define COUNT_CARD(i)	(cardCnt[i]++)
#endif
#else
#define COUNT_CARD(i)	{}
#define COUNT_CARD1(i)	{}
#define COUNT_CARD2(i)	{}
#endif


/** DEBUG **/
#ifdef  BO_DEBUG
PVT void ScanMem (Word_t *start, Word_t *stop, int gen, int objKind)
{
    bibop_t	    bibop = BIBOP;
    Word_t	    w;
    int		    indx;
    aid_t	    aid;
    bigobj_region_t *region;
    bigobj_desc_t   *dp;

    while (start < stop) {
	w = *start;
	if (isBOXED(w)) {
	    int		indx = BIBOP_ADDR_TO_INDEX(w);
	    aid_t	id = bibop[indx];
	    switch (EXTRACT_OBJC(id)) {
	      case OBJC_bigobj:
		while (!BO_IS_HDR(id)) {
		    id = bibop[--indx];
		}
		region = (bigobj_region_t *)BIBOP_INDEX_TO_ADDR(indx);
		dp = ADDR_TO_BODESC(region, w);
		if (dp->state == BO_FREE) {
		    SayDebug ("** [%d/%d]: %#x --> %#x; unexpected free big-object\n",
			gen, objKind, start, w);
		}
		break;
	      case OBJC_record:
	      case OBJC_pair:
	      case OBJC_string:
	      case OBJC_array:
		break;
	      default:
		if (id != AID_UNMAPPED)
		    SayDebug ("** [%d/%d]: %#x --> %#x; strange object class %d\n",
			gen, objKind, start, w, EXTRACT_OBJC(id));
		break;
	    }
	}
	start++;
    }
}
#endif /** BO_DEBUG **/

/* local routines */
PVT void MajorGC_ScanRoots (
	ml_state_t *msp, heap_t *heap, ml_val_t **roots, int maxCollectedGen);
PVT void MajorGC_SweepToSpace (heap_t *heap, int maxCollectedGen, int maxSweptGen);
PVT bool_t MajorGC_SweepToSpArrays (
	heap_t *heap, int maxGen, arena_t *tosp, card_map_t *cm);
PVT ml_val_t MajorGC_ForwardObj (
	heap_t *heap, aid_t maxAid, ml_val_t obj, aid_t id);
PVT bigobj_desc_t *MajorGC_ForwardBigObj (
	heap_t *heap, int maxGen, ml_val_t obj, aid_t id);
PVT ml_val_t MajorGC_FwdSpecial (
	heap_t *heap, aid_t maxAid, ml_val_t *obj, aid_t id, ml_val_t desc);

/* the symbolic names of the arenas */
char		*ArenaName[NUM_ARENAS+1] = {
	"new", "record", "pair", "string", "array"
    };
/* DEBUG */PVT char *StateName[] = {"FREE", "YOUNG", "FORWARD", "OLD", "PROMOTE"};

/* Check a word for a from-space reference */
#ifdef TOSPACE_ID
#define NO_GC_INLINE /* DEBUG */
#endif
#ifndef NO_GC_INLINE
#define MajorGC_CheckWord(heap,bibop,maxAid,p)	{			\
	ml_val_t	__w = *(p);					\
	if (isBOXED(__w)) {						\
	    aid_t	__aid = ADDR_TO_PAGEID(bibop, __w);		\
IFBO_COUNT1(__aid);							\
	    if (IS_FROM_SPACE(__aid,maxAid)) {				\
		*(p) = MajorGC_ForwardObj(heap, maxAid, __w, __aid);	\
	    }								\
        }								\
    }
#else
PVT void MajorGC_CheckWord (heap_t *heap, bibop_t bibop, aid_t maxAid, ml_val_t *p)
{
    ml_val_t	w = *(p);
    if (isBOXED(w)) {
	aid_t	arena_id = ADDR_TO_PAGEID(bibop, w);
IFBO_COUNT1(arena_id);							\
	if (IS_FROM_SPACE(arena_id, maxAid)) {
	    *(p) = MajorGC_ForwardObj(heap, maxAid, w, arena_id);
	}
#ifdef TOSPACE_ID
	else if (IS_TOSPACE_AID(arena_id)) {
	    Die ("CheckWord: TOSPACE reference: %#x (%#x) --> %#x\n",
		p, ADDR_TO_PAGEID(bibop, p), w);
	}
#endif
    }
}
#endif


/* MajorGC:
 *
 * Do a garbage collection of (at least) the first level generations.
 * By definition, level should be at least 1.
 */
void MajorGC (ml_state_t *msp, ml_val_t **roots, int level)
{
    heap_t	*heap = msp->ml_heap;
    bibop_t	bibop = BIBOP;
    int		i, j;
    int		maxCollectedGen;	/* the oldest generation being collected */
    int		maxSweptGen;
#ifdef GC_STATS
    ml_val_t	*tospTop[NUM_ARENAS]; /* for counting # of bytes forwarded */
#endif

#ifndef PAUSE_STATS	/* don't do timing when collecting pause data */
    StartGCTimer(msp->ml_vproc);
#endif
#ifdef BO_REF_STATS
numBO1 = numBO2 = numBO3 = 0;
#endif

  /* Flip to-space and from-space */
    maxCollectedGen = Flip (heap, level);
    if (maxCollectedGen < heap->numGens) {
	maxSweptGen = maxCollectedGen+1;
#ifdef GC_STATS
      /* Remember the top of to-space for maxSweptGen */
	for (i = 0;  i < NUM_ARENAS;  i++)
	    tospTop[i] = heap->gen[maxSweptGen-1]->arena[i]->nextw;
#endif /* GC_STATS */
    }
    else {
	maxSweptGen = maxCollectedGen;
    }
    NUM_GC_GENS(maxCollectedGen);	/* record pause info */

#ifdef VM_STATS
    ReportVM (msp, maxCollectedGen);
#endif

#ifndef PAUSE_STATS	/* don't do messages when collecting pause data */
    if (GCMessages) {
	SayDebug ("GC #");
	for (i = heap->numGens-1;  i >= 0; i--) {
	    SayDebug ("%d.", heap->gen[i]->numGCs);
	}
	SayDebug ("%d:  ", heap->numMinorGCs);
    }
#endif

    HeapMon_StartGC (heap, maxCollectedGen);

  /* Scan the roots */
    MajorGC_ScanRoots (msp, heap, roots, maxCollectedGen);

  /* Sweep to-space */
    MajorGC_SweepToSpace (heap, maxCollectedGen, maxSweptGen);

  /* Handle weak pointers */
    if (heap->weakList != NIL(ml_val_t *))
	ScanWeakPtrs (heap);

  /* reclaim from-space; we do this from oldest to youngest so that
   * we can promote big objects.
   */
    for (i = maxCollectedGen;  i > 0;  i--) {
	gen_t		*gen = heap->gen[i-1], *promoteGen;
	int		forwardState, promoteState;

	FreeGeneration (heap, i-1);
#ifdef TOSPACE_ID
	for (j = 0;  j < NUM_ARENAS;  j++) {
	    arena_t	*ap = gen->arena[j];
	    if (isACTIVE(ap))
		MarkRegion (bibop, ap->tospBase, ap->tospSizeB, ap->id);
	}
#endif
      /* NOTE: there should never be any big-objects in the oldest generation
       * with the BO_PROMOTE tag.
       */
	if (i != heap->numGens) {
	    promoteGen = heap->gen[i];
	    forwardState = BO_OLD;
	  /* the objects promoted from generation i to generation i+1, when
	   * generation i+1 is also being collected, are "OLD", thus we need
	   * to mark the corresponding big objects as old so that they do not
	   * get out of sync.  Since the oldest generation has only YOUNG
	   * objects, we have to check for that case too.
	   */
	    if ((i == maxCollectedGen) || (i == heap->numGens-1))
		promoteState = BO_YOUNG;
	    else
		promoteState = BO_OLD;
	}
	else {
	  /* oldest generation objects are promoted to the same generation */
	    promoteGen = heap->gen[i-1];
	    forwardState = BO_YOUNG; /* oldest gen has only YOUNG objects */
	}
	for (j = 0;  j < NUM_BIGOBJ_KINDS;  j++) {
	    bigobj_desc_t   *dp, *dq, *forward, *promote;
	    promote = promoteGen->bigObjs[j];
	    forward = NIL(bigobj_desc_t *);
	    for (dp = gen->bigObjs[j];  dp != NIL(bigobj_desc_t *);  ) {
		dq = dp->next;
		ASSERT(dp->gen == i);
		switch (dp->state) {
		  case BO_YOUNG:
		  case BO_OLD:
		    BO_Free (heap, dp);
		    break;
		  case BO_FORWARD:
		    dp->state = forwardState;
		    dp->next = forward;
		    forward = dp;
		    break;
		  case BO_PROMOTE:
		    dp->state = promoteState;
		    dp->next = promote;
		    dp->gen++;
		    promote = dp;
		    break;
		  default:
		    Die ("strange bigobject state %d @ %#x in generation %d\n",
			dp->state, dp, i);
		} /* end switch */
		dp = dq;
	    }
	    promoteGen->bigObjs[j] = promote; /* a nop for the oldest generation */
	    gen->bigObjs[j] = forward;
	}
    }
#ifdef BO_DEBUG
/** DEBUG **/
for (i = 0;  i < heap->numGens;  i++) {
gen_t	*gen = heap->gen[i];
ScanMem((Word_t *)(gen->arena[RECORD_INDX]->tospBase), (Word_t *)(gen->arena[RECORD_INDX]->nextw), i+1, RECORD_INDX);
ScanMem((Word_t *)(gen->arena[PAIR_INDX]->tospBase), (Word_t *)(gen->arena[PAIR_INDX]->nextw), i+1, PAIR_INDX);
ScanMem((Word_t *)(gen->arena[ARRAY_INDX]->tospBase), (Word_t *)(gen->arena[ARRAY_INDX]->nextw), i+1, ARRAY_INDX);
}
/** DEBUG **/
#endif

  /* relabel BIBOP entries for big-object regions to reflect promotions */
    {
	bigobj_region_t	*rp;
	bigobj_desc_t	*dp;
	int		min;

	for (rp = heap->bigRegions;  rp != NIL(bigobj_region_t *);  rp = rp->next) {
	  /* if the minimum generation of the region is less than or equal
	   * to maxCollectedGen, then it is possible that it has increased
	   * as a result of promotions or freeing of objects.
	   */
	    if (rp->minGen <= maxCollectedGen) {
		min = MAX_NUM_GENS;
		for (i = 0;  i < rp->nPages; ) {
		    dp = rp->objMap[i];
		    if ((! BO_IS_FREE(dp)) && (dp->gen < min))
			min = dp->gen;
		    i += BO_NUM_BOPAGES(dp);
		}
		if (rp->minGen != min) {
		    rp->minGen = min;
		    MarkRegion (bibop, (ml_val_t *)rp, MEMOBJ_SZB(rp->memObj),
			AID_BIGOBJ(min));
		    bibop[BIBOP_ADDR_TO_INDEX(rp)] = AID_BIGOBJ_HDR(min);
		}
	    }
	} /* end for */
    }

  /* remember the top of to-space in the collected generations */
    for (i = 0;  i < maxCollectedGen;  i++) {
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
    if (maxSweptGen != maxCollectedGen) {
	gen_t	*gen = heap->gen[maxSweptGen-1];
	for (j = 0;  j < NUM_ARENAS;  j++) {
	    CNTR_INCR(&(heap->numCopied[maxSweptGen-1][j]),
		gen->arena[j]->nextw - tospTop[j]);
	}
    }
    for (i = 0;  i < maxCollectedGen;  i++) {
	for (j = 0;  j < NUM_ARENAS;  j++) {
	    arena_t	*ap = heap->gen[i]->arena[j];
	    if (isACTIVE(ap)) {
		CNTR_INCR(&(heap->numCopied[i][j]), ap->nextw - tospTop[j]);
	    }
	}
    }
#endif

#ifdef BO_REF_STATS
SayDebug ("bigobj stats: %d seen, %d lookups, %d forwarded\n",
numBO1, numBO2, numBO3);
#endif
#ifndef PAUSE_STATS	/* don't do timing when collecting pause data */
    if (GCMessages) {
	long	gcTime;
	StopGCTimer (msp->ml_vproc, &gcTime);
	SayDebug (" (%d ms)\n", gcTime);
    }
    else
	StopGCTimer (msp->ml_vproc, NIL(long *));
#endif

#ifdef VM_STATS
    ReportVM (msp, 0);
#endif

#ifdef CHECK_HEAP
    CheckHeap(heap, maxSweptGen);
#endif

} /* end of MajorGC. */


/* MajorGC_ScanRoots:
 */
PVT void MajorGC_ScanRoots (
    ml_state_t	*msp,
    heap_t	*heap,
    ml_val_t	**roots,
    int		maxCollectedGen)
{
    bibop_t	bibop = BIBOP;
    aid_t	maxAid = MAKE_MAX_AID(maxCollectedGen);
    ml_val_t	*rp;
    int		i;

    while ((rp = *roots++) != NIL(ml_val_t *)) {
	MajorGC_CheckWord(heap, bibop, maxAid, rp);
    }

  /* Scan the dirty cards in the older generations */
    for (i = maxCollectedGen;  i < heap->numGens;  i++) {
	gen_t	    *gen = heap->gen[i];
#ifdef COUNT_CARDS
#ifndef BIT_CARDS
/*CARD*/cardCnt1[i]=cardCnt2[i]=0;
#else
/*CARD*/cardCnt[i]=0;
#endif
#endif
	if (isACTIVE(gen->arena[ARRAY_INDX])) {
	    card_map_t	*cm = gen->dirty;
	    if (cm != NIL(card_map_t *)) {
		ml_val_t    *maxSweep = gen->arena[ARRAY_INDX]->sweep_nextw;
		int	    card;
#if (!defined(BIT_CARDS) && defined(TOSPACE_ID))
		FOR_DIRTY_CARD (cm, maxCollectedGen, card, {
		    ml_val_t	*p = (cm->baseAddr + (card*CARD_SZW));
		    ml_val_t	*q = p + CARD_SZW;
		    int		mark = i+1;
COUNT_CARD1(i);
		    if (q > maxSweep)
		      /* don't sweep above the allocation high-water mark */
			q = maxSweep;
		    for (;  p < q;  p++) {
			ml_val_t	w = *p;
			if (isBOXED(w)) {
			    aid_t	aid = ADDR_TO_PAGEID(bibop, w);
			    int		targetGen;
IFBO_COUNT1(aid);
			    if (IS_FROM_SPACE(aid, maxAid)) {
			      /* this is a from-space object */
			        if (IS_BIGOBJ_AID(aid)) {
				    bigobj_desc_t	*dp;
				    dp = MajorGC_ForwardBigObj (
					heap, maxCollectedGen, w, aid);
				    targetGen = dp->gen;
			        }
			        else {
				    *p =
				    w = MajorGC_ForwardObj(heap, maxAid, w, aid);
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
		    else if (i == maxCollectedGen)
			cm->map[card] = CARD_CLEAN;
		});
#elif (!defined(BIT_CARDS))
		FOR_DIRTY_CARD (cm, maxCollectedGen, card, {
		    ml_val_t	*p = (cm->baseAddr + (card*CARD_SZW));
		    ml_val_t	*q = p + CARD_SZW;
		    int		mark = i+1;
COUNT_CARD1(i);
		    if (q > maxSweep)
		      /* don't sweep above the allocation high-water mark */
			q = maxSweep;
		    for (;  p < q;  p++) {
			ml_val_t	w = *p;
			if (isBOXED(w)) {
			    aid_t	aid = ADDR_TO_PAGEID(bibop, w);
			    int		targetGen;
IFBO_COUNT1(aid);
			    if (IS_FROM_SPACE(aid, maxAid)) {
			      /* this is a from-space object */
COUNT_CARD2(i);
			        if (IS_BIGOBJ_AID(aid)) {
				    bigobj_desc_t	*dp;
				    dp = MajorGC_ForwardBigObj (
					heap, maxCollectedGen, w, aid);
				    targetGen = dp->gen;
			        }
			        else {
				    *p =
				    w = MajorGC_ForwardObj(heap, maxAid, w, aid);
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
		    else if (i == maxCollectedGen)
			cm->map[card] = CARD_CLEAN;
		});
#else
		FOR_DIRTY_CARD (cm, card, {
		    ml_val_t	*p = (cm->baseAddr + (card*CARD_SZW));
		    ml_val_t	*q = p + CARD_SZW;
COUNT_CARD(i);
		    if (q > maxSweep)
		      /* don't sweep above the allocation high-water mark */
			q = maxSweep;
		    for (;  p < q;  p++) {
			MajorGC_CheckWord (heap, bibop, maxAid, p);
		    }
		});
#endif
	    }
	}
    } /* end of for */

#ifdef COUNT_CARDS
/*CARD*/SayDebug ("\n[%d] SWEEP: ", maxCollectedGen);
/*CARD*/for(i = maxCollectedGen;  i < heap->numGens;  i++) {
/*CARD*/  card_map_t  *cm = heap->gen[i]->dirty;
/*CARD*/  if (i > maxCollectedGen) SayDebug (", ");
#ifndef BIT_CARDS
/*CARD*/  SayDebug ("[%d] %d/%d/%d", i+1, cardCnt1[i], cardCnt2[i],
/*CARD*/	(cm != NIL(card_map_t*)) ? cm->numCards : 0);
#else
/*CARD*/  SayDebug ("[%d] %d/%d", i+1, cardCnt[i],
/*CARD*/	(cm != NIL(card_map_t*)) ? cm->numCards : 0);
#endif
/*CARD*/}
/*CARD*/SayDebug ("\n");
#endif

} /* end of MajorGC_ScanRoots */


/* MajorGC_SweepToSpace:
 * Sweep the to-space arenas.  Because there are few references forward in time, we
 * try to completely scavenge a younger generation before moving on to the
 * next oldest.
 */
PVT void MajorGC_SweepToSpace (heap_t *heap, int maxCollectedGen, int maxSweptGen)
{
    int		i;
    bool_t	swept;
    bibop_t	bibop = BIBOP;
    aid_t	maxAid = MAKE_MAX_AID(maxCollectedGen);

#define SweepToSpArena(gen, indx)	{					\
	arena_t	    *__ap = (gen)->arena[(indx)];				\
	if (isACTIVE(__ap)) {							\
	    ml_val_t    *__p, *__q;						\
	    __p = __ap->sweep_nextw;						\
	    if (__p < __ap->nextw) {						\
		swept = TRUE;							\
		do {								\
		    for (__q = __ap->nextw;  __p < __q;  __p++) {		\
			MajorGC_CheckWord(heap, bibop, maxAid, __p);		\
		    }								\
		} while (__q != __ap->nextw);					\
		__ap->sweep_nextw = __q;					\
	    }									\
	}									\
    } /* SweepToSpArena */

    do {
	swept = FALSE;
	for (i = 0;  i < maxSweptGen;  i++) {
	    gen_t	*gen = heap->gen[i];

	  /* Sweep the record and pair arenas */
	    SweepToSpArena(gen, RECORD_INDX);
	    SweepToSpArena(gen, PAIR_INDX);

	  /* Sweep the array arena */
	    {
		arena_t		*ap = gen->arena[ARRAY_INDX];
		if (isACTIVE(ap)
		&& MajorGC_SweepToSpArrays (heap, maxCollectedGen, ap, gen->dirty))
		    swept = TRUE;
	    }
	}
    } while (swept);

}/* end of SweepToSpace */


/* MajorGC_SweepToSpArrays:
 *
 * Sweep the to-space of the array arena, returning true if any objects
 * are actually swept.
 */
PVT bool_t MajorGC_SweepToSpArrays (
	heap_t *heap, int maxGen, arena_t *tosp, card_map_t *cm)
{
    ml_val_t	w, *p, *stop;
    int		thisGen;
    Word_t	cardMask = ~(CARD_SZB - 1);
    aid_t	*bibop = BIBOP;
    aid_t	maxAid = MAKE_MAX_AID(maxGen);
#ifndef BIT_CARDS
    ml_val_t	*cardStart;
    int		cardMark;
#endif

  /* Sweep a single card at a time, looking for references that need to
   * be remembered.
   */
    thisGen = EXTRACT_GEN(tosp->id);
    p = tosp->sweep_nextw;
    if (p == tosp->nextw)
	return FALSE;
    while (p < tosp->nextw) {
	stop = (ml_val_t *)(((Addr_t)p + CARD_SZB) & cardMask);
	if (stop > tosp->nextw)
	    stop = tosp->nextw;
      /* Sweep the next page until we see a reference to a younger generation */
#ifndef BIT_CARDS
	cardStart = p;
	cardMark = CARD(cm, cardStart);
#endif
	while (p < stop) {
	    if (isBOXED(w = *p)) {
		aid_t		arena_id = ADDR_TO_PAGEID(bibop, w);
		int		targetGen;

IFBO_COUNT1(arena_id);
		if (IS_FROM_SPACE(arena_id, maxAid)) {
		  /* this is a from-space object */
		    if (IS_BIGOBJ_AID(arena_id)) {
			bigobj_desc_t	*dp;
			dp = MajorGC_ForwardBigObj (heap, maxGen, w, arena_id);
			targetGen = dp->gen;
		    }
		    else {
			*p = w = MajorGC_ForwardObj(heap, maxAid, w, arena_id);
#ifdef TOSPACE_ID
			{ aid_t aid = ADDR_TO_PAGEID(bibop, w);
			  if (IS_TOSPACE_AID(aid))
			    targetGen = TOSPACE_GEN(aid);
			  else
			    targetGen = EXTRACT_GEN(aid);
			}
#else
			targetGen = EXTRACT_GEN(ADDR_TO_PAGEID(bibop, w));
#endif
		    }
#ifndef BIT_CARDS
		    if (targetGen < cardMark)
			cardMark = targetGen;
#else
		    if (targetGen < thisGen) {
		      /* the forwarded object is in a younger generation */
			MARK_CARD(cm, p);
		      /* finish the card up quickly */
			for (p++; p < stop;  p++) {
			    MajorGC_CheckWord(heap, bibop, maxAid, p);
			}
			break;
		    }
#endif
		}
#ifdef TOSPACE_ID
		else if (IS_TOSPACE_AID(arena_id)) {
		    Die ("Sweep Arrays: TOSPACE reference: %#x (%#x) --> %#x\n",
			p, ADDR_TO_PAGEID(bibop, p), w);
		}
#endif
	    }
	    p++;
	} /* end of while */
#ifndef BIT_CARDS
	if (cardMark < thisGen)
	    MARK_CARD(cm, cardStart, cardMark);
#endif
    } /* end of while */
    tosp->sweep_nextw = p;

    return TRUE;

} /* end of MajorGC_SweepToSpArrays */


/* MajorGC_ForwardObj:
 *
 * Forward an object.
 */
PVT ml_val_t MajorGC_ForwardObj (heap_t *heap, aid_t maxAid, ml_val_t v, aid_t id)
{
    ml_val_t	*obj = PTR_MLtoC(ml_val_t, v);
    ml_val_t	*new_obj;
    ml_val_t	desc;
    Word_t	len;
    arena_t	*arena;

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
	  case DTAG_record:
	    len = GET_LEN(desc);
	    break;
	  default:
	    Die ("bad record tag %d, obj = %#x, desc = %#x",
		GET_TAG(desc), obj, desc);
	} /* end of switch */
	arena = heap->gen[EXTRACT_GEN(id)-1]->arena[RECORD_INDX];
	if (isOLDER(arena, obj))
	    arena = arena->nextGen;
      } break;

      case OBJC_pair: {
	ml_val_t	w;

	w = obj[0];
	if (isDESC(w))
	    return PTR_CtoML(FOLLOW_FWDPAIR(w, obj));
	else {
	  /* forward the pair */
	    arena = heap->gen[EXTRACT_GEN(id)-1]->arena[PAIR_INDX];
	    if (isOLDER(arena, obj))
		arena = arena->nextGen;
	    new_obj = arena->nextw;
	    arena->nextw += PAIR_SZW;
	    new_obj[0] = w;
	    new_obj[1] = obj[1];
	  /* setup the forward pointer in the old pair */
	    obj[0] =  MAKE_PAIR_FP(new_obj);
	    return PTR_CtoML(new_obj);
	}
      } break;

      case OBJC_string: {
	arena = heap->gen[EXTRACT_GEN(id)-1]->arena[STRING_INDX];
	if (isOLDER(arena, obj))
	    arena = arena->nextGen;
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
	} /* end of switch */
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
	    return MajorGC_FwdSpecial (heap, maxAid, obj, id, desc);
	  default:
	    Die ("bad array tag %d, obj = %#x, desc = %#x",
		GET_TAG(desc), obj, desc);
	} /* end of switch */
	arena = heap->gen[EXTRACT_GEN(id)-1]->arena[ARRAY_INDX];
	if (isOLDER(arena, obj))
	    arena = arena->nextGen;
      } break;

      case OBJC_bigobj:
	MajorGC_ForwardBigObj (heap, EXTRACT_GEN(maxAid), v, id);
	return v;

      default:
	Die("unknown object class %d @ %#x", EXTRACT_OBJC(id), obj);
    } /* end of switch */

  /* Allocate and initialize a to-space copy of the object */
    new_obj = arena->nextw;
    arena->nextw += (len + 1);
    *new_obj++ = desc;
    ASSERT(arena->nextw <= arena->tospTop);
    COPYLOOP(obj, new_obj, len);

  /* set up the forward pointer, and return the new object. */
    obj[-1] = DESC_forwarded;
    obj[0] = (ml_val_t)(Addr_t)new_obj;

    return PTR_CtoML(new_obj);

} /* end of MajorGC_ForwardObj */


/* MajorGC_ForwardBigObj:
 *
 * Forward a big-object obj, where id is the BIBOP entry for obj.
 * Return the descriptor for obj.
 */
PVT bigobj_desc_t *MajorGC_ForwardBigObj (
	heap_t *heap, int maxGen, ml_val_t obj, aid_t id)
{
    int		    i;
    bigobj_region_t *region;
    bigobj_desc_t   *dp;

BO2_COUNT;
    for (i = BIBOP_ADDR_TO_INDEX(obj);  !BO_IS_HDR(id);  id = BIBOP[--i])
	continue;
    region = (bigobj_region_t *)BIBOP_INDEX_TO_ADDR(i);
    dp = ADDR_TO_BODESC(region, obj);
    if ((dp->gen <= maxGen) && BO_IS_FROM_SPACE(dp)) {
BO3_COUNT;
      /* forward the big-object; note that objects in the oldest generation
       * will always be YOUNG, thus will never be promoted.
       */
	if (dp->state == BO_YOUNG)
	    dp->state = BO_FORWARD;
	else
	    dp->state = BO_PROMOTE;
    }

    return dp;

} /* end of MajorGC_ForwardBigObj */


/* MajorGC_FwdSpecial:
 *
 * Forward a special object (suspension, weak pointer, ...).
 */
PVT ml_val_t MajorGC_FwdSpecial (
    heap_t	*heap,
    aid_t	maxAid,
    ml_val_t	*obj,
    aid_t	id,
    ml_val_t	desc
)
{
    gen_t	*gen = heap->gen[EXTRACT_GEN(id)-1];
    arena_t	*arena = gen->arena[ARRAY_INDX];
    ml_val_t	*new_obj;

    if (isOLDER(arena, obj))
	arena = arena->nextGen;

  /* allocate the new object */
    new_obj = arena->nextw;
    arena->nextw += SPECIAL_SZW;  /* all specials are two words */

    switch (GET_LEN(desc)) {
      case SPCL_evaled_susp:
      case SPCL_unevaled_susp:
      case SPCL_null_weak:
	*new_obj++ = desc;
	*new_obj = *obj;
	break;
      case SPCL_weak: {
	    ml_val_t	v = *obj;
#ifdef DEBUG_WEAK_PTRS
SayDebug ("MajorGC: weak [%#x ==> %#x] --> %#x", obj, new_obj+1, v);
#endif
	    if (! isBOXED(v)) {
#ifdef DEBUG_WEAK_PTRS
SayDebug (" unboxed\n");
#endif
	      /* weak references to unboxed objects are never nullified */
		*new_obj++ = DESC_weak;
		*new_obj = v;
	    }
	    else {
		aid_t		aid = ADDR_TO_PAGEID(BIBOP, v);
		ml_val_t	*vp = PTR_MLtoC(ml_val_t, v);
		ml_val_t	desc;

		if (IS_FROM_SPACE(aid, maxAid)) {
		    switch (EXTRACT_OBJC(aid)) {
		      case OBJC_record:
		      case OBJC_string:
		      case OBJC_array:
			desc = vp[-1];
			if (desc == DESC_forwarded) {
			  /* Reference to an object that has already been forwarded.
			   * NOTE: we have to put the pointer to the non-forwarded
			   * copy of the object (i.e, v) into the to-space copy
			   * of the weak pointer, since the GC has the invariant
			   * it never sees to-space pointers during sweeping.
			   */
#ifdef DEBUG_WEAK_PTRS
SayDebug (" already forwarded to %#x\n", FOLLOW_FWDOBJ(vp));
#endif
			    *new_obj++ = DESC_weak;
			    *new_obj = v;
			}
			else {
			  /* the forwarded version of weak objects are threaded
			   * via their descriptor fields.  We mark the object
			   * reference field to make it look like an unboxed value,
			   * so that the to-space sweeper does not follow the weak
			   * reference.
			   */
#ifdef DEBUG_WEAK_PTRS
SayDebug (" forward (start = %#x)\n", vp);
#endif
			    *new_obj = MARK_PTR(PTR_CtoML(gen->heap->weakList));
			    gen->heap->weakList = new_obj++;
			    *new_obj = MARK_PTR(vp);
			}
			break;
		      case OBJC_pair:
			if (isDESC(desc = vp[0])) {
			  /* Reference to a pair that has already been forwarded.
			   * NOTE: we have to put the pointer to the non-forwarded
			   * copy of the pair (i.e, v) into the to-space copy
			   * of the weak pointer, since the GC has the invariant
			   * it never sees to-space pointers during sweeping.
			   */
#ifdef DEBUG_WEAK_PTRS
SayDebug (" (pair) already forwarded to %#x\n", FOLLOW_FWDPAIR(desc, vp));
#endif
			    *new_obj++ = DESC_weak;
			    *new_obj = v;
			}
			else {
			    *new_obj = MARK_PTR(PTR_CtoML(gen->heap->weakList));
			    gen->heap->weakList = new_obj++;
			    *new_obj = MARK_PTR(vp);
			}
			break;
		      case OBJC_bigobj:
			Die ("weak big object");
			break;
		    }
		}
		else {
		  /* reference to an older object */
#ifdef DEBUG_WEAK_PTRS
SayDebug (" old object\n");
#endif
		    *new_obj++ = DESC_weak;
		    *new_obj = v;
		}
	    }
	} break;
      default:
	Die ("strange/unexpected special object @ %#x; desc = %#x\n", obj, desc);
    } /* end of switch */

    obj[-1] = DESC_forwarded;
    obj[0] = (ml_val_t)(Addr_t)new_obj;

    return PTR_CtoML(new_obj);

} /* end of MajorGC_FwdSpecial */
