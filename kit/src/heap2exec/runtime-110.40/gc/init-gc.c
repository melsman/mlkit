/* init-gc.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * The GC initialization code.
 *
 */

#ifdef PAUSE_STATS		/* GC pause statistics are UNIX dependent */
#  include "ml-unixdep.h"
#endif

#include <stdarg.h>
#include "ml-base.h"
#include "ml-options.h"
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

PVT int		DfltRatios[MAX_NUM_GENS] = {
	DFLT_RATIO1,	DFLT_RATIO2,	DFLT_RATIO,	DFLT_RATIO,
	DFLT_RATIO,	DFLT_RATIO,	DFLT_RATIO
    };

#ifdef TWO_LEVEL_MAP
#  error two level map not supported
#else
aid_t		*BIBOP;
#endif

#ifdef COLLECT_STATS /** should this go into gc-stats.c ??? **/
bool_t		StatsOn = TRUE;	/* if TRUE, then generate stats */
int		StatsFD = -1;	/* the file descriptor to write the data to */
stat_rec_t	StatsBuf[STATS_BUF_SZ];	/* buffer of data */
int		NStatsRecs;	/* the number of records in the buffer */
#endif


/* ParseHeapParams:
 *
 * Parse and heap parameters from the command-line arguments.
 */
heap_params_t *ParseHeapParams (char **argv)
{
    char	    option[MAX_OPT_LEN], *optionArg;
    bool_t	    errFlg = FALSE;
    char	    *arg;
    heap_params_t   *params;

    if ((params = NEW_OBJ(heap_params_t)) == NIL(heap_params_t *)) {
	Die("unable to allocate heap_params");
    }

  /* We use 0 or "-1" to signify that the default value should be used. */
    params->allocSz = 0;
    params->numGens = -1;
    params->cacheGen = -1;

#define MATCH(opt)	(strcmp(opt, option) == 0)
#define CHECK(opt)	{						\
	if (optionArg[0] == '\0') {					\
	    errFlg = TRUE;						\
	    Error("missing argument for \"%s\" option\n", opt);		\
	    continue;							\
	}								\
    } /* CHECK */

    while ((arg = *argv++) != NIL(char *)) {
	if (isRuntimeOption(arg, option, &optionArg)) {
	    if (MATCH("alloc")) { /* set allocation size */
		CHECK("alloc");
		if ((params->allocSz = GetSzOption(ONE_K, optionArg)) < 0) {
		    errFlg = TRUE;
		    Error ("bad argument for \"@SMLalloc\" option\n");
		}
	    }
	    else if (MATCH("ngens")) {
		CHECK("ngens");
		params->numGens = atoi(optionArg);
		if (params->numGens < 1)
		    params->numGens = 1;
		else if (params->numGens > MAX_NGENS)
		    params->numGens = MAX_NGENS;
	    }
	    else if (MATCH("vmcache")) {
		CHECK("vmcache");
		params->cacheGen = atoi(optionArg);
		if (params->cacheGen < 0)
		    params->cacheGen = 0;
		else if (params->cacheGen > MAX_NGENS)
		    params->cacheGen = MAX_NGENS;
	    }
	}
	if (errFlg)
	    return NIL(heap_params_t *);
    } /* while */

    return params;

} /* end of ParseHeapParams */

/* InitHeap:
 *
 * Create and initialize the heap.
 */
void InitHeap (ml_state_t *msp, bool_t isBoot, heap_params_t *params)
{
    int		i, j, ratio, max_sz;
    heap_t	*heap;
    gen_t	*gen;
    mem_obj_t	*baseObj;
    ml_val_t	*allocBase;

    if (params->allocSz == 0) params->allocSz = DFLT_ALLOC;
    if (params->numGens < 0) params->numGens = DFLT_NGENS;
    if (params->cacheGen < 0) params->cacheGen = DFLT_CACHE_GEN;

  /* First we initialize the underlying memory system */
    MEM_InitMemory ();

  /* allocate the base memory object (holds the BIBOP and allocation space) */
    {
	long	bibopSz;

#ifdef TWO_LEVEL_MAP
#  error two level map not supported
#else
	bibopSz = BIBOP_SZ * sizeof(aid_t);
#endif
	baseObj = MEM_AllocMemObj (MAX_NUM_PROCS*params->allocSz + bibopSz);
	if (baseObj == NIL(mem_obj_t *))
	    Die ("unable to allocate memory object for BIBOP");
	BIBOP = (bibop_t)MEMOBJ_BASE(baseObj);
	allocBase = (ml_val_t *)(((Addr_t)BIBOP) + bibopSz);
    }

  /* initialize the BIBOP */
#ifdef TWO_LEVEL_MAP
#  error two level map not supported
#else
    for (i = 0;  i < BIBOP_SZ;  i++)
	BIBOP[i] = AID_UNMAPPED;
#endif

  /* initialize heap descriptor */
    heap = NEW_OBJ(heap_t);
    memset ((char *)heap, 0, sizeof(heap_t));
    for (i = 0;  i < MAX_NUM_GENS;  i++) {
	ratio = DfltRatios[i];
	if (i == 0)
	    max_sz = MAX_SZ1(params->allocSz * MAX_NUM_PROCS);
	else
/** NOTE: the following is bogus **/
	    max_sz = (ratio * heap->gen[i-1]->arena[0]->maxSizeB) / 2;
	gen		=
	heap->gen[i]	= NEW_OBJ(gen_t);
	gen->heap	= heap;
	gen->genNum	= i+1;
	gen->numGCs	= 0;
	gen->lastPrevGC	= 0;
	gen->ratio	= ratio;
	gen->toObj	= NIL(mem_obj_t *);
	gen->fromObj	= NIL(mem_obj_t *);
	gen->cacheObj	= NIL(mem_obj_t *);
	gen->dirty	= NIL(card_map_t *);
	for (j = 0;  j < NUM_ARENAS;  j++) {
	    gen->arena[j] = NEW_OBJ(arena_t);
	    gen->arena[j]->tospSizeB = 0;
	    gen->arena[j]->reqSizeB = 0;
	    gen->arena[j]->maxSizeB = max_sz;
	    gen->arena[j]->id = MAKE_AID(i+1, j+1, 0);
	}
	for (j = 0;  j < NUM_BIGOBJ_KINDS;  j++)
	    gen->bigObjs[j] = NIL(bigobj_desc_t *);
    }
    for (i = 0;  i < params->numGens;  i++) {
	int	k = (i == params->numGens-1) ? i : i+1;
	for (j = 0;  j < NUM_ARENAS;  j++)
	    heap->gen[i]->arena[j]->nextGen = heap->gen[k]->arena[j];
    }
    heap->numGens		= params->numGens;
    heap->cacheGen		= params->cacheGen;
    heap->numMinorGCs		= 0;
    heap->numBORegions		= 0;
    heap->bigRegions		= NIL(bigobj_region_t *);
    heap->freeBigObjs		= NEW_OBJ(bigobj_desc_t);
    heap->freeBigObjs->obj	= (Addr_t)0;
    heap->freeBigObjs->sizeB	= 0;
    heap->freeBigObjs->state	= BO_FREE;
    heap->freeBigObjs->prev	= heap->freeBigObjs;
    heap->freeBigObjs->next	= heap->freeBigObjs;
    heap->weakList		= NIL(ml_val_t *);

  /* initialize new space */
    heap->baseObj = baseObj;
    heap->allocBase = allocBase;
    heap->allocSzB = MAX_NUM_PROCS*params->allocSz;
    MarkRegion (BIBOP, (ml_val_t *)BIBOP, MEMOBJ_SZB(heap->baseObj), AID_NEW);
#ifdef VERBOSE
    SayDebug ("NewSpace = [%#x, %#x:%#x), %d bytes\n",
	heap->allocBase, HEAP_LIMIT(heap),
	(Word_t)(heap->allocBase)+params->allocSz, params->allocSz);
#endif

#ifdef GC_STATS
    ClearGCStats (heap);
#endif
#if defined(COLLECT_STATS)
    if (StatsFD > 0) {
	stat_hdr_t	hdr;
	CNTR_ZERO(&(heap->numAlloc));
	hdr.mask = STATMASK_ALLOC|STATMASK_NGENS|STATMASK_START|STATMASK_STOP;
	hdr.isNewRuntime = 1;
	hdr.allocSzB = params->allocSz;
	hdr.numGens = params->numGens;
	gettimeofday (&(hdr.startTime), NIL(struct timezone *));
	write (StatsFD, (char *)&hdr, sizeof(stat_hdr_t));
    }
#endif

    if (isBoot) {
      /* Create the first generation's to-space. */
	for (i = 0;  i < NUM_ARENAS;  i++)
	    heap->gen[0]->arena[i]->tospSizeB = RND_MEMOBJ_SZB(2 * heap->allocSzB);
	if (NewGeneration(heap->gen[0]) == FAILURE)
	    Die ("unable to allocate initial first generation space\n");
	for (i = 0;  i < NUM_ARENAS;  i++)
	    heap->gen[0]->arena[i]->oldTop = heap->gen[0]->arena[i]->tospBase;
    }

  /* initialize the GC related parts of the ML state */
    msp->ml_heap	= heap;
    msp->ml_allocPtr	= (ml_val_t *)(msp->ml_allocArena);
#ifdef SOFT_POLL
    ResetPollLimit (msp);
#else
    msp->ml_limitPtr	= HEAP_LIMIT(heap);
#endif

} /* end of InitHeap */


#ifdef GC_STATS
/* ClearGCStats:
 */
void ClearGCStats (heap_t *heap)
{
    int		i, j;

    CNTR_ZERO(&(heap->numAlloc));
    for (i = 0;  i < MAX_NUM_GENS;  i++) {
	for (j = 0;  j < NUM_ARENAS;  j++) {
	    CNTR_ZERO(&(heap->numCopied[i][j]));
	}
    }

} /* end of ClearStats */
#endif

