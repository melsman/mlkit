/* gc-stats.h
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 */

#ifndef _GC_STATS_
#define _GC_STATS_

#include "stats-data.h"

#ifdef VM_STATS
extern void ReportVM (ml_state_t *msp, int maxCollectedGen);
#endif

#ifdef PAUSE_STATS

#define START_GC_PAUSE(HEAP)	{					\
	if (StatsOn) {							\
	    heap_t	    *__heap = (HEAP);				\
	    stat_rec_t	    *__p = &(StatsBuf[NStatsRecs]);		\
	    Unsigned32_t    __n = (Addr_t)(msp->ml_allocPtr) - 		\
				    (Addr_t)(__heap->allocBase);	\
	    CNTR_INCR(&(__heap->numAlloc), __n);			\
	    __p->allocCnt = __heap->numAlloc;				\
	    __p->numGens = 0;						\
	    gettimeofday(&(__p->startTime), NIL(struct timezone *));	\
	}								\
    }

#define NUM_GC_GENS(NGENS)		{				\
	if (StatsOn)							\
	    StatsBuf[NStatsRecs].numGens = (NGENS);			\
    }

#define STOP_GC_PAUSE()			{				\
	if (StatsOn) {							\
	    gettimeofday(&(StatsBuf[NStatsRecs].stopTime),		\
		NIL(struct timezone *));				\
	    STATS_FINISH();						\
	}								\
    }

#else /* !PAUSE_STATS */
#define START_GC_PAUSE(HEAP)
#define NUM_GC_GENS(NGENS)
#define STOP_GC_PAUSE()
#endif /* PAUSE_STATS */

#endif
