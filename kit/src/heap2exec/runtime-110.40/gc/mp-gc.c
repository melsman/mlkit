/* mp-gc.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 *
 * Extra routines to support GC in the MP implementation.
 *
 */

#include <sys/time.h>
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
#include "ml-mp.h"
#include "vproc-state.h"

/* MP_SUPPORT */

/* PartitionAllocArena:
 *
 * Divide this allocation arena into smaller disjoint arenas for
 * use by the parallel processors.
 */
void PartitionAllocArena (vproc_state_t *vsps[])
{
    int		indivSz;
    ml_val_t	*aBase;
    int		i;
    int pollFreq = INT_MLtoC(DEREF(PollFreq));
    ml_state_t  *msp, *msp0;

    msp0 = vsps[0]->vp_state;
    indivSz = msp0->ml_heap->allocSzB / MAX_NUM_PROCS;
    aBase = msp0->ml_heap->allocBase;
    for (i = 0; i < MAX_NUM_PROCS; i++) {
	msp = vsps[i]->vp_state;
#ifdef MP_DEBUG
SayDebug ("vsps[%d]->vp_state-> (ml_allocPtr %x/ml_limitPtr %x) changed to ",
i, msp->ml_allocPtr, msp->ml_limitPtr);
#endif
	msp->ml_heap = msp0->ml_heap;
	msp->ml_allocPtr = aBase;
	msp->ml_realLimit = HEAP_LIMIT_SIZE(aBase, indivSz);

#ifdef MP_GCPOLL
	if (pollFreq > 0) {
#ifdef MP_DEBUG
SayDebug ("(with PollFreq=%d) ", pollFreq);
#endif
	    msp->ml_limitPtr = aBase + pollFreq*POLL_GRAIN_CPSI;
	    msp->ml_limitPtr =
		(msp->ml_limitPtr > msp->ml_realLimit)
		    ? msp->ml_realLimit
		    : msp->ml_limitPtr;

	}
	else {
	    msp->ml_limitPtr = msp->ml_realLimit;
	}
#else
	msp->ml_limitPtr = HEAP_LIMIT_SIZE(aBase,indivSz);
#endif

#ifdef MP_DEBUG
SayDebug ("%x/%x\n",msp->ml_allocPtr, msp->ml_limitPtr);
#endif
	aBase = (ml_val_t *) (((Addr_t) aBase) + indivSz);
    }

} /* end of PartitionAllocArena */


PVT volatile int    MP_RdyForGC = 0;	/* the number of processors that are */
					/* ready for the GC. */
PVT int		    MPCollectorProc;	/* the processor that does the GC */

/* extra roots provided by InvokeGCWithRoots go here */
ml_val_t            *mpExtraRoots[NUM_EXTRA_ROOTS*MAX_NUM_PROCS];
PVT ml_val_t        **mpExtraRootsPtr;

/* MP_StartCollect:
 *
 * Waits for all procs to check in and chooses one to do the 
 * collect (MPCollectorProc).  MPCollectorProc returns to the invoking
 * collection function and does the collect while the other procs
 * wait at a barrier. MPCollectorProc will eventually check into this
 * barrier releasing the waiting procs.
 */
int MP_StartCollect (ml_state_t *msp)
{
    int		nProcs;
    vproc_state_t *vsp = msp->ml_vproc;

    MP_SetLock(MP_GCLock);
    if (MP_RdyForGC++ == 0) {
        mpExtraRoots[0] = NIL(ml_val_t *);
        mpExtraRootsPtr = mpExtraRoots;
#ifdef MP_GCPOLL
	ASSIGN(PollEvent, ML_true);
#ifdef MP_DEBUG
	SayDebug ("%d: set poll event\n", msp->ml_mpSelf);
#endif
#endif
      /* we're the first one in, we'll do the collect */
	MPCollectorProc = vsp->vp_mpSelf;
#ifdef MP_DEBUG
	SayDebug ("MPCollectorProc is %d\n",MPCollectorProc);
#endif
    }
    MP_UnsetLock(MP_GCLock);

    {
#ifdef MP_DEBUG
	int n = 0;
#endif
      /* nb: some other proc can be concurrently acquiring new processes */
	while (MP_RdyForGC !=  (nProcs = MP_ActiveProcs())) {
	  /* spin */
#ifdef MP_DEBUG
	    if (n == 10000000) {
		n = 0;
		SayDebug ("%d spinning %d <> %d <alloc=0x%x, limit=0x%x>\n", 
		    msp->ml_mpSelf, MP_RdyForGC, nProcs, msp->ml_allocPtr,
		    msp->ml_limitPtr);
	    }
	    else
	      n++;
#endif
	}
    }

  /* Here, all of the processors are ready to do GC */

#ifdef MP_GCPOLL
    ASSIGN(PollEvent, ML_false);
#ifdef MP_DEBUG
    SayDebug ("%d: cleared poll event\n", msp->ml_mpSelf);
#endif
#endif
#ifdef MP_DEBUG
    SayDebug ("(%d) all %d/%d procs in\n", msp->ml_mpSelf, MP_RdyForGC, MP_ActiveProcs());
#endif
    if (MPCollectorProc != vsp->vp_mpSelf) {
#ifdef MP_DEBUG
	SayDebug ("%d entering barrier %d\n",vsp->vp_mpSelf,nProcs);
#endif
	MP_Barrier(MP_GCBarrier, nProcs);
    
#ifdef MP_DEBUG
	SayDebug ("%d left barrier\n", vsp->vp_mpSelf);
#endif
	return 0;
    }

    return nProcs;

} /* end of MP_StartCollect */


/* MP_StartCollectWithRoots:
 *
 * as above, but collects extra roots into mpExtraRoots
 */
int MP_StartCollectWithRoots (ml_state_t *msp, va_list ap)
{
    int		nProcs;
    ml_val_t    *p;
    vproc_state_t *vsp = msp->ml_vproc;

    MP_SetLock(MP_GCLock);
    if (MP_RdyForGC++ == 0) {
        mpExtraRootsPtr = mpExtraRoots;
#ifdef MP_GCPOLL
	ASSIGN(PollEvent, ML_true);
#ifdef MP_DEBUG
	SayDebug ("%d: set poll event\n", vsp->vp_mpSelf);
#endif
#endif
      /* we're the first one in, we'll do the collect */
	MPCollectorProc = vsp->vp_mpSelf;
#ifdef MP_DEBUG
	SayDebug ("MPCollectorProc is %d\n",MPCollectorProc);
#endif
    }
    while ((p = va_arg(ap, ml_val_t *)) != NIL(ml_val_t *)) {
	*mpExtraRootsPtr++ = p;
    }
    *mpExtraRootsPtr = p;  /* NIL(ml_val_t *) */
    MP_UnsetLock(MP_GCLock);

    {
#ifdef MP_DEBUG
	int n = 0;
#endif
      /* nb: some other proc can be concurrently acquiring new processes */
	while (MP_RdyForGC !=  (nProcs = MP_ActiveProcs())) {
	  /* spin */
#ifdef MP_DEBUG
	    if (n == 10000000) {
		n = 0;
		SayDebug ("%d spinning %d <> %d <alloc=0x%x, limit=0x%x>\n", 
		    vsp->vp_mpSelf, MP_RdyForGC, nProcs, msp->ml_allocPtr,
		    msp->ml_limitPtr);
	    }
	    else
	      n++;
#endif
	}
    }

  /* Here, all of the processors are ready to do GC */

#ifdef MP_GCPOLL
    ASSIGN(PollEvent, ML_false);
#ifdef MP_DEBUG
    SayDebug ("%d: cleared poll event\n", msp->ml_mpSelf);
#endif
#endif
#ifdef MP_DEBUG
    SayDebug ("(%d) all %d/%d procs in\n", msp->ml_vproc->vp_mpSelf, MP_RdyForGC, MP_ActiveProcs());
#endif
    if (MPCollectorProc != vsp->vp_mpSelf) {
#ifdef MP_DEBUG
	SayDebug ("%d entering barrier %d\n", vsp->vp_mpSelf, nProcs);
#endif
	MP_Barrier(MP_GCBarrier, nProcs);
    
#ifdef MP_DEBUG
	SayDebug ("%d left barrier\n", vsp->vp_mpSelf);
#endif
	return 0;
    }

    return nProcs;

} /* end of MP_StartCollectWithRoots */


/* MP_FinishCollect:
 */
void MP_FinishCollect (ml_state_t *msp, int n)
{
  /* this works, but PartitionAllocArena is overkill */
    PartitionAllocArena(VProc);
    MP_SetLock(MP_GCLock);
#ifdef MP_DEBUG
    SayDebug ("%d entering barrier %d\n", msp->ml_vproc->vp_mpSelf,n);
#endif
    MP_Barrier(MP_GCBarrier,n);
    MP_RdyForGC = 0;

#ifdef MP_DEBUG
    SayDebug ("%d left barrier\n", msp->ml_vproc->vp_mpSelf);
#endif
    MP_UnsetLock(MP_GCLock);

} /* end of MP_FinishCollect */

