/* flip.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * This code determines which generations to flip and what the
 * to-space sizes should be.
 */

#include "ml-base.h"
#include "ml-limits.h"
#include "ml-state.h"
#include "heap.h"
#include "heap-monitor.h"

#if defined(VERBOSE)
extern char	*ArenaName[NUM_ARENAS+1];
#endif


/* Flip:
 *
 * Determine which generations need to be flipped and flip them.  Return
 * the number of flipped generations (which will be at least min_gc_level).
 * It is assumed that the fist generation is always flipped (i.e., that
 * min_gc_level > 1).
 */
int Flip (heap_t *heap, int min_gc_level)
{
    int		i, j, prevGC, numGCs;
    Addr_t	newSz, prevOldSz[NUM_ARENAS], minSize[NUM_ARENAS];
    arena_t	*ap;

#ifdef VERBOSE
SayDebug ("Flip: min_gc_level = %d\n", min_gc_level);
#endif
    for (i = 0;  i < NUM_ARENAS;  i++)
	prevOldSz[i] = heap->allocSzB;

    prevGC = heap->numMinorGCs;
    for (i = 0;  i < heap->numGens;  i++) {
	gen_t	*g = heap->gen[i];

      /* Check to see if generation (i+1) should be flipped */
#ifdef VERBOSE
SayDebug ("checking generation %d\n", i+1);
#endif
	if (i >= min_gc_level) {
	    for (j = 0;  j < NUM_ARENAS; j++) {
		arena_t	*ap = g->arena[j];
#ifdef VERBOSE
SayDebug ("  %s: avail = %d, prev = %d\n",
ArenaName[j+1], (isACTIVE(ap) ? AVAIL_SPACE(ap) : 0), prevOldSz[j]);
#endif
		if ((isACTIVE(ap) ? AVAIL_SPACE(ap) : 0) < prevOldSz[j])
		    goto flip;
	    }
	  /* Here we don't need to flip gen[i] */
	    return i;
	}
      flip:; /* Here we need to flip gen[i] */

	numGCs = prevGC - g->lastPrevGC;
#ifdef VERBOSE
SayDebug ("Flip generation %d: (%d GCs)\n", i+1, numGCs);
#endif
      /* Compute the space requirements for this generation, make the old
       * to-space into from-space, and allocate a new to-space.
       */
	for (j = 0;  j < NUM_ARENAS;  j++) {
	    Addr_t	minSz, thisMinSz;
	    ap = g->arena[j];
	    if (isACTIVE(ap)) {
		FLIP_ARENA(ap);
		HeapMon_MarkFromSp (heap, ap->frspBase, ap->frspSizeB);
		thisMinSz = ((Addr_t)(ap->frspTop) - (Addr_t)(ap->oldTop));
	    }
	    else {
		ap->frspSizeB = 0;  /* to insure accurate stats */
		if ((ap->reqSizeB == 0) && (prevOldSz[j] == 0))
		    continue;
		else
		    thisMinSz = 0;
	    }
	    minSz = prevOldSz[j] + thisMinSz + ap->reqSizeB;
	    if (j == PAIR_INDX)
	      /* first slot isn't used, but may need the space for poly = */
		minSz += 2*WORD_SZB;
	    minSize[j] = minSz;

#ifdef OLD_POLICY
	  /* The desired size is the minimum size times the ratio for the arena,
	   * but it shouldn't exceed the maximum size for the arena (unless
	   * minSz > maxSizeB).
	   */
	    newSz = (ap->ratio * minSz) / RATIO_UNIT;
	    if (newSz < minSz+ap->reqSizeB)
		newSz = minSz+ap->reqSizeB;
#endif
	  /* The desired size is one that will allow "ratio" GCs of the
	   * previous generation before this has to be collected again.
	   * We approximate this as ((f*ratio) / n), where
	   *   f == # of bytes forwarded since the last collection of this generation
	   *   n == # of collections of the previous generation since the last
	   *        collection of this generation
	   * We also need to allow space for young objects in this generation,
	   * but the new size shouldn't exceed the maximum size for the arena
	   * (unless minSz > maxSizeB).
	   */
	    newSz = minSz + ((thisMinSz * (g->ratio-1)) / numGCs);
#ifdef VERBOSE
SayDebug ("  %s: min = %d, prev = %d, thisMin = %d, req = %d, new = %d, max = %d\n",
ArenaName[j+1], minSz, prevOldSz[j], thisMinSz, ap->reqSizeB, newSz, ap->maxSizeB);
#endif
	    if (newSz > ap->maxSizeB)
		newSz = (minSz > ap->maxSizeB) ? minSz : ap->maxSizeB;
	    ap->reqSizeB = 0;

	    if (newSz > 0) {
		ap->tospSizeB = RND_MEMOBJ_SZB(newSz);
#ifdef VERBOSE
SayDebug ("    alloc %d\n", ap->tospSizeB);
#endif
	    }
	    else {
		ap->nextw = NIL(ml_val_t *);
		ap->tospTop = NIL(ml_val_t *);
		ap->tospSizeB = 0;
	    }
	  /* Note: any data between ap->oldTop and ap->nextw is "young", and
	   * should stay in this generation.
	   */
	    if (ap->frspSizeB > 0)
		prevOldSz[j] = (Addr_t)(ap->oldTop) - (Addr_t)(ap->frspBase);
	    else
		prevOldSz[j] = 0;
	}

	g->lastPrevGC = prevGC;
	g->numGCs++;
	prevGC = g->numGCs;
	g->fromObj = g->toObj;
	if (NewGeneration(g) == FAILURE) {
	  /* try to allocate the minimum size */
	    Error ("unable to allocate to-space for generation %d; trying smaller size\n", i+1);
	    for (j = 0;  j < NUM_ARENAS;  j++) {
		g->arena[j]->tospSizeB = RND_MEMOBJ_SZB(minSize[j]);
	    }
	    if (NewGeneration(g) == FAILURE)
		Die("unable to allocate minimum size\n");
	}
#ifdef TOSPACE_ID
	for (j = 0;  j < NUM_ARENAS;  j++) {
	    arena_t	*ap = g->arena[j];
	    if (isACTIVE(ap))
		MarkRegion (BIBOP, ap->tospBase, ap->tospSizeB, TOSPACE_AID(i+1));
	}
#endif

	if (isACTIVE(g->arena[ARRAY_INDX]))
	    NewDirtyVector(g);
    }

    return heap->numGens;

} /* end of Flip */
