/* gc-stats.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * Support routines for gathering GC statistics.
 */

#include "ml-osdep.h"
#include <sys/time.h>
#include <sys/resource.h>
#include "ml-base.h"
#include "ml-limits.h"
#include "ml-state.h"
#include "heap.h"
#include "cntr.h"
#include "gc-stats.h"

FILE			*DebugF;
FILE			*StatsF;

/** Virtual memory statistics **/
#ifdef VM_STATS

#define ROUND1K(X)      (((X)+512)/1024)

/* ReportVM:
 *
 */
void ReportVM (ml_state_t *msp, int maxCollectedGen)
{
    heap_t		*heap = msp->ml_heap;
    int                 kbytesPerPage = GETPAGESIZE()/1024;
    FILE		*f = (StatsF == NULL) ? DebugF : StatsF;
    struct rusage       ru;
    Addr_t		bytesAllocated, oldBytes, vmAlloc;
    int			i, j;

    getrusage(RUSAGE_SELF, &ru);

#ifdef XXX
    bytesAllocated = ((Addr_t)(msp->ml_allocPtr) - (Addr_t)(heap->allocBase));
#else
    bytesAllocated = 0;
#endif

  /* count size of older generations */
    oldBytes = 0;
    for (i = 0;  i < heap->numGens;  i++) {
	for (j = 0;  j < NUM_ARENAS;  j++) {
	    arena_t	*ap = heap->gen[i]->arena[j];
	    if (i < maxCollectedGen) {
		if (ap->frspSizeB > 0)
		    oldBytes += ((Addr_t)(ap->frspTop) - (Addr_t)(ap->frspBase));
	    }
	    else {
		if (isACTIVE(ap))
		    oldBytes += ((Addr_t)(ap->nextw) - (Addr_t)(ap->tospBase));
	    }
	}
      /* count code objects too! */
	for (j = 0;  j < NUM_BIGOBJ_KINDS;  j++) {
	    bigobj_desc_t   *dp = heap->gen[i]->bigObjs[j];
	    for (; dp != NIL(bigobj_desc_t *);  dp = dp->next) {
		oldBytes += dp->sizeB;
	    }
	}
    }

  /* get amount of allocated VM (in Kb) */
    vmAlloc = MEM_GetVMSize();

    fprintf (f, "VM{alloc=");
    CNTR_FPRINTF (f, &(heap->numAlloc), 10);
    fprintf (f, ", new=%dk, old=%dk, tot=%dk, max_rss=%d}\n",
        ROUND1K(bytesAllocated),
        ROUND1K(oldBytes),
        vmAlloc,
        ru.ru_maxrss*kbytesPerPage);
    fflush (f);
 
} /* end of ReportVM */

#endif


/** Pause time statistics **/
#ifdef PAUSE_STATS

pause_info_t		PauseTbl[MAX_NGENS+1];

/* InitPauseTbl:
 */
void InitPauseTbl ()
{
    int			i;

    for (i = 0;  i <= MAX_NGENS;  i++) {
	PauseTbl[i].numGCs = 0;
	PauseTbl[i].maxPause = 0;
	PauseTbl[i].buckets = NIL(short *);
    }

    GrowPauseTbl (0, MS_TO_BUCKET(500));
    GrowPauseTbl (1, MS_TO_BUCKET(1000));
    GrowPauseTbl (2, MS_TO_BUCKET(2000));
    GrowPauseTbl (3, MS_TO_BUCKET(3000));
    GrowPauseTbl (4, MS_TO_BUCKET(3000));
    GrowPauseTbl (5, MS_TO_BUCKET(4000));
    GrowPauseTbl (6, MS_TO_BUCKET(4000));

} /* end of InitPauseTbl */

/* GrowPauseTbl:
 *
 */
void GrowPauseTbl (int gen, int pause)
{
    pause_info_t	*p = &(PauseTbl[gen]);
    short		*buckets = p->buckets, *new;
    int			sz, i;

    for (sz = (p->maxPause ? p->maxPause : 16);  sz < pause;  sz = sz+sz)
	continue;
    new = NEW_VEC(short, sz);

    if (buckets != NIL(short *)) {
	for (i = 0;  i < p->maxPause;  i++)
	    new[i] = buckets[i];
	for (; i < sz; i++)
	    new[i] = 0;
	FREE (buckets);
    }
    else {
	for (i = 0;  i < sz;  i++)
	    new[i] = 0;
    }

    p->buckets = new;
    p->maxPause = sz;

} /* end of GrowPauseTbl */

/* ReportPauses:
 *
 */
void ReportPauses (FILE *f)
{
    pause_info_t    *infop;
    int		    i, j, k, n, maxPause;

  /* compute the largest maxPause time */
    maxPause = 100; /* one second */
    for (i = MAX_NGENS;  i > 0;  i--) {
	infop = &(PauseTbl[i]);
	if ((infop->numGCs > 0) && (infop->maxPause > maxPause)) {
	    for (j = infop->maxPause-1;  (j > maxPause) && (infop->buckets[j] == 0);  j--)
		continue;
	    if (j > maxPause)
		maxPause = ((j+99)/100)*100;
	}
    }

    fprintf(f, "newgraph\n");
    fprintf(f, "  xaxis\n");
    fprintf(f, "    label : GC Pause Times (ms)\n");
    fprintf(f, "    no_auto_hash_marks\n");
    fprintf(f, "    size 4.5\n");
    fprintf(f, "    min -10 max %d\n", maxPause);
    for (i = 0;  i <= maxPause; i += 50)
	fprintf(f, "    hash_at %4d hash_label at %4d : %4d\n", i, i, i*10);
    fprintf(f, "  yaxis\n");
    fprintf(f, "    label : Number of pauses\n");
    fprintf(f, "    min 0\n");

    for (i = MAX_NGENS;  i > 0;  i--) {
	infop = &(PauseTbl[i]);
	if (infop->numGCs == 0)
	    continue;
	fprintf(f, "  (* generation %d pause times *)\n", i);
	fprintf(f, "    newcurve\n");
	fprintf(f, "      label : Generation %d\n", i);
	fprintf(f, "      marktype xbar\n");
	fprintf(f, "      pts\n");
	for (j = 0;  j < infop->maxPause;  j++) {
	    if (infop->buckets[j] == 0)
		continue;
	    for (k = 1, n = 0;  k <= i;  k++) {
		if (PauseTbl[k].maxPause >= j)
		    n += PauseTbl[k].buckets[j];
	    }
	    fprintf(f, "        %4d %3d\n", j*10, n);
	}
    }

    fflush (f);

} /* end of ReportPauses */

#endif
