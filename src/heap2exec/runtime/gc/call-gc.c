/* call-gc.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * The main interface between the GC and the rest of the run-time system.
 * These are the routines used to invoke the GC.
 */

#ifdef PAUSE_STATS		/* GC pause statistics are UNIX dependent */
#  include "ml-unixdep.h"
#endif

#include <stdarg.h>
#include "ml-base.h"
#include "ml-limits.h"
#include "memory.h"
#include "ml-state.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "cntr.h"
#include "heap.h"
#include "heap-monitor.h"
#include "ml-globals.h"
#include "ml-timer.h"
#include "gc-stats.h"
#include "vproc-state.h"
#include "profile.h"

#ifdef C_CALLS
/* This is a list of pointers into the C heap locations that hold
 * pointers to ML functions. This list is not part of any ML data
 * structure(s).  (also see gc/major-gc.c and c-libs/c-calls/c-calls-fns.c)
 */
extern ml_val_t		CInterfaceRootList;
#endif

#ifdef COUNT_REG_MASKS
#define NUM_MASKS	64
PVT struct {
    Word_t	mask;
    int		count;
}	    Masks[NUM_MASKS];
PVT	    NumMasks = 0;
PVT	    NumOthers = 0;

PVT void RecordMask (Word_t m)
{
    int		i;

    for (i = 0;  (i < NumMasks) && (Masks[i].mask != m);  i++)
	continue;
    if (i < NumMasks)
	Masks[i].count++;
    else if (i < NUM_MASKS) {
	NumMasks++;
	Masks[i].mask = m;
	Masks[i].count = 1;
    }
    else
	NumOthers++;
}

void DumpMasks ()
{
    int		i;

    SayDebug ("GC Masks:\n");
    for (i = 0;  i < NumMasks;  i++) {
	SayDebug ("  %#8x: %5d\n", Masks[i].mask, Masks[i].count);
    }
    if (NumOthers > 0)
	SayDebug ("  ????????:  %5d\n", NumOthers);
}
#endif /* COUNT_REG_MASKS */



/* InvokeGC:
 *
 * Invoke a garbage collection.  A garbage collection always involves
 * collecting the allocation space.  In addition, if level is greater than
 * 0, or if the first generation is full after the minor collection, then
 * a major collection of one or more generations is performed (at least
 * level generations are collected).
 */
void InvokeGC (ml_state_t *msp, int level)
{
    ml_val_t	*roots[NUM_GC_ROOTS];	/* registers and globals */
    ml_val_t	**rootsPtr = roots;
    heap_t	*heap;
    Word_t	mask;
    int		i;
#ifdef MP_SUPPORT
    int		nProcs;
#endif

    ASSIGN(ProfCurrent, PROF_MINOR_GC);

#ifdef MP_SUPPORT
#ifdef MP_DEBUG
    SayDebug ("igc %d\n", msp->ml_mpSelf);
#endif
    if ((nProcs = MP_StartCollect (msp)) == 0) {
      /* a waiting proc */
	ASSIGN(ProfCurrent, PROF_RUNTIME);
	return;
    }
#endif

    START_GC_PAUSE(msp->ml_heap);

#ifdef C_CALLS
    *rootsPtr++ = &CInterfaceRootList;
#endif

#ifdef MP_SUPPORT
  /* get extra roots from procs that entered through InvokeGCWithRoots */
    for (i = 0;  mpExtraRoots[i] != NIL(ml_val_t *); i++)
	*rootsPtr++ = mpExtraRoots[i];
#endif

  /* Gather the roots */
    for (i = 0;  i < NumCRoots;  i++)
	*rootsPtr++ = CRoots[i];
#ifdef MP_SUPPORT
    {
	vproc_state_t   *vsp;
	ml_state_t	*msp;
	int		j;
      
	for (j = 0; j < MAX_NUM_PROCS; j++) {
	    vsp = VProc[j];
	    msp = vsp->vp_state;
#ifdef MP_DEBUG
	SayDebug ("msp[%d] alloc/limit was %x/%x\n",
	    j, msp->ml_allocPtr, msp->ml_limitPtr);
#endif
	    if (vsp->vp_mpState == MP_PROC_RUNNING) {
		*rootsPtr++ = &(msp->ml_exnCont);
		*rootsPtr++ = &(msp->ml_varReg);
#ifdef BASE_INDX
		*rootsPtr++ = &(msp->ml_baseReg);
#endif
		mask = msp->ml_liveRegMask;
		for (i = 0;  mask != 0;  i++, mask >>= 1) {
		    if ((mask & 1) != 0)
			*rootsPtr++ = &(msp->ml_roots[ArgRegMap[i]]);
		} /* for */
#ifdef N_PSEUDO_REGS
		for (i = 0; i < N_PSEUDO_REGS; i++)
		    *rootsPtr++ = &(msp->ml_pseudoRegs[i]);
#endif
	    }
	} /* for */
    }
#else /* !MP_SUPPORT */
    *rootsPtr++ = &(msp->ml_exnCont);
    *rootsPtr++ = &(msp->ml_varReg);
#ifdef BASE_INDX
    *rootsPtr++ = &(msp->ml_baseReg);
#endif
    mask = msp->ml_liveRegMask;
#ifdef COUNT_REG_MASKS
    RecordMask (mask);
#endif
    for (i = 0;  mask != 0;  i++, mask >>= 1) {
	if ((mask & 1) != 0)
	    *rootsPtr++ = &(msp->ml_roots[ArgRegMap[i]]);
    }
#ifdef N_PSEUDO_REGS
    for (i = 0; i < N_PSEUDO_REGS; i++)
	*rootsPtr++ = &(msp->ml_pseudoRegs[i]);
#endif
#endif /* MP_SUPPORT */
    *rootsPtr = NIL(ml_val_t *);

    MinorGC (msp, roots);

    heap = msp->ml_heap;

  /* Check for major GC */
    if (level == 0) {
	gen_t	*gen1 = heap->gen[0];
	Word_t	sz = msp->ml_allocArenaSzB;

	for (i = 0;  i < NUM_ARENAS;  i++) {
	    arena_t *arena = gen1->arena[i];
	    if (isACTIVE(arena) && (AVAIL_SPACE(arena) < sz)) {
		level = 1;
		break;
	    }
	}
    }

    if (level > 0) {
#ifdef MP_SUPPORT
	vproc_state_t   *vsp;
	ml_state_t	*msp;

	for (i = 0; i < MAX_NUM_PROCS; i++) {
	    vsp = VProc[i];
	    msp = vsp->vp_state;
	    if (vsp->vp_mpState == MP_PROC_RUNNING)
		*rootsPtr++ = &(msp->ml_pc);
	}
#else
	ASSIGN(ProfCurrent, PROF_MAJOR_GC);
	*rootsPtr++ = &(msp->ml_pc);
#endif
	*rootsPtr = NIL(ml_val_t *);
	MajorGC (msp, roots, level);
    }
    else {
	HeapMon_UpdateHeap (heap, 1);
    }

  /* reset the allocation space */
#ifdef MP_SUPPORT
    MP_FinishCollect (msp, nProcs);
#else
    msp->ml_allocPtr	= heap->allocBase;
#ifdef SOFT_POLL
    ResetPollLimit (msp);
#else
    msp->ml_limitPtr    = HEAP_LIMIT(heap);
#endif
#endif

    STOP_GC_PAUSE();

    ASSIGN(ProfCurrent, PROF_RUNTIME);

} /* end of InvokeGC */


/* InvokeGCWithRoots:
 *
 * Invoke a garbage collection with possible additional roots.  The list of
 * additional roots should be NIL terminated.  A garbage collection always
 * involves collecting the allocation space.  In addition, if level is greater
 * than 0, or if the first generation is full after the minor collection, then
 * a major collection of one or more generations is performed (at least level
 * generations are collected).
 *
 * NOTE: the MP version of this may be broken, since if a processor calls this
 * but isn't the collecting process, then the extra roots are lost.
 */
void InvokeGCWithRoots (ml_state_t *msp, int level, ...)
{
    ml_val_t	*roots[NUM_GC_ROOTS+NUM_EXTRA_ROOTS];	/* registers and globals */
    ml_val_t	**rootsPtr = roots, *p;
    heap_t	*heap;
    Word_t	mask;
    int		i;
    va_list	ap;
#ifdef MP_SUPPORT
    int		nProcs;
#endif

    ASSIGN(ProfCurrent, PROF_MINOR_GC);

#ifdef MP_SUPPORT
#ifdef MP_DEBUG
    SayDebug ("igcwr %d\n", msp->ml_mpSelf);
#endif
    va_start (ap, level);
    nProcs = MP_StartCollectWithRoots (msp, ap);
    va_end(ap);
    if (nProcs == 0)
	ASSIGN(ProfCurrent, PROF_RUNTIME);
	return; /* a waiting proc */
#endif

    START_GC_PAUSE(msp->ml_heap);

#ifdef C_CALLS
    *rootsPtr++ = &CInterfaceRootList;
#endif

#ifdef MP_SUPPORT
  /* get extra roots from procs that entered through InvokeGCWithRoots.
   * Our extra roots were placed in mpExtraRoots by MP_StartCollectWithRoots.
   */
    for (i = 0; mpExtraRoots[i] != NIL(ml_val_t *); i++)
	*rootsPtr++ = mpExtraRoots[i];
#else
  /* record extra roots from param list */
    va_start (ap, level);
    while ((p = va_arg(ap, ml_val_t *)) != NIL(ml_val_t *)) {
	*rootsPtr++ = p;
    }
    va_end(ap);
#endif /* MP_SUPPORT */

  /* Gather the roots */
    for (i = 0;  i < NumCRoots;  i++)
	*rootsPtr++ = CRoots[i];
#ifdef MP_SUPPORT
    {
	ml_state_t	*msp;
	vproc_state_t   *vsp;
	int		j;
      
	for (j = 0; j < MAX_NUM_PROCS; j++) {
	    vsp = VProc[j];
	    msp = vsp->vp_state;
#ifdef MP_DEBUG
	SayDebug ("msp[%d] alloc/limit was %x/%x\n",
	    j, msp->ml_allocPtr, msp->ml_limitPtr);
#endif
	    if (vsp->vp_mpState == MP_PROC_RUNNING) {
		*rootsPtr++ = &(msp->ml_exnCont);
		*rootsPtr++ = &(msp->ml_varReg);
#ifdef BASE_INDX
		*rootsPtr++ = &(msp->ml_baseReg);
#endif
		mask = msp->ml_liveRegMask;
		for (i = 0;  mask != 0;  i++, mask >>= 1) {
		    if ((mask & 1) != 0)
			*rootsPtr++ = &(msp->ml_roots[ArgRegMap[i]]);
		} /* for */
#ifdef N_PSEUDO_REGS
                for (i = 0; i < N_PSEUDO_REGS; i++)
                    *rootsPtr++ = &(msp->ml_pseudoRegs[i]);
#endif
	    }
	} /* for */
    }
#else /* !MP_SUPPORT */
    *rootsPtr++ = &(msp->ml_exnCont);
    *rootsPtr++ = &(msp->ml_varReg);
#ifdef BASE_INDX
    *rootsPtr++ = &(msp->ml_baseReg);
#endif
    mask = msp->ml_liveRegMask;
    for (i = 0;  mask != 0;  i++, mask >>= 1) {
	if ((mask & 1) != 0)
	    *rootsPtr++ = &(msp->ml_roots[ArgRegMap[i]]);
    }
#ifdef N_PSEUDO_REGS
    for (i = 0; i < N_PSEUDO_REGS; i++)
	*rootsPtr++ = &(msp->ml_pseudoRegs[i]);
#endif
#endif /* MP_SUPPORT */
    *rootsPtr = NIL(ml_val_t *);

    MinorGC (msp, roots);

    heap = msp->ml_heap;

  /* Check for major GC */
    if (level == 0) {
	gen_t	*gen1 = heap->gen[0];
	Word_t	sz = msp->ml_allocArenaSzB;

	for (i = 0;  i < NUM_ARENAS;  i++) {
	    arena_t *arena = gen1->arena[i];
	    if (isACTIVE(arena) && (AVAIL_SPACE(arena) < sz)) {
		level = 1;
		break;
	    }
	}
    }

    if (level > 0) {
#ifdef MP_SUPPORT
	vproc_state_t   *vsp;

	for (i = 0; i < MAX_NUM_PROCS; i++) {
	    vsp = VProc[i];
	    if (vsp->vp_mpState == MP_PROC_RUNNING)
		*rootsPtr++ = &(vsp->vp_state->ml_pc);
	}
#else
	ASSIGN(ProfCurrent, PROF_MAJOR_GC);
	*rootsPtr++ = &(msp->ml_pc);
#endif
	*rootsPtr = NIL(ml_val_t *);
	MajorGC (msp, roots, level);
    }
    else {
	HeapMon_UpdateHeap (heap, 1);
    }

  /* reset the allocation space */
#ifdef MP_SUPPORT
    MP_FinishCollect (msp, nProcs);
#else
    msp->ml_allocPtr	= heap->allocBase;
#ifdef SOFT_POLL
    ResetPollLimit (msp);
#else
    msp->ml_limitPtr    = HEAP_LIMIT(heap);
#endif
#endif

    STOP_GC_PAUSE();

    ASSIGN(ProfCurrent, PROF_RUNTIME);

} /* end of InvokeGCWithRoots */

/* NeedGC:
 *
 * Check to see if a GC is required, or if there is enough heap space for
 * nbytes worth of allocation.  Return TRUE, if GC is required, FALSE
 * otherwise.
 */
bool_t NeedGC (ml_state_t *msp, Word_t nbytes)
{
#if (defined(MP_SUPPORT) && defined(COMMENT_MP_GCPOLL))
    if ((((Addr_t)(msp->ml_allocPtr)+nbytes) >= (Addr_t)(msp->ml_limitPtr))
    || (INT_MLtoC(PollEvent) != 0))
#elif defined(MP_SUPPORT)
    if (((Addr_t)(msp->ml_allocPtr)+nbytes) >= (Addr_t)(msp->ml_limitPtr))
#else
    if (((Addr_t)(msp->ml_allocPtr)+nbytes) >= (Addr_t)HEAP_LIMIT(msp->ml_heap))
#endif
	return TRUE;
    else
	return FALSE;

} /* end of NeedGC */


#ifdef SOFT_POLL
/* ResetPollLimit:
 *
 * Reset the limit pointer according to the current polling frequency.
 */
void ResetPollLimit (ml_state_t *msp)
{
    int		pollFreq = INT_MLtoC(DEREF(PollFreq));
    heap_t	*heap = msp->ml_heap;

  /* assumes ml_allocPtr has been reset */
    msp->ml_realLimit	= HEAP_LIMIT(heap);
    if (pollFreq > 0) {
	msp->ml_limitPtr  = heap->allocBase + pollFreq*POLL_GRAIN_CPSI;
	msp->ml_limitPtr  = (msp->ml_limitPtr > msp->ml_realLimit)
	    ? msp->ml_realLimit
	    : msp->ml_limitPtr;
    }
    else
	msp->ml_limitPtr  = msp->ml_realLimit;

} /* end ResetPollLimit */
#endif /* SOFT_POLL */
