/* heap-monitor.h
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * The interface of an X-windows heap monitor.
 */

#ifndef _HEAP_MONITOR_
#define _HEAP_MONITOR_

#ifndef _ML_BASE_
#include "ml-base.h"
#endif


#ifdef HEAP_MONITOR

typedef struct monitor monitor_t;

extern void HeapMon_StartGC (heap_t *heap, int maxCollectedGen);
extern void HeapMon_UpdateHeap (heap_t *heap, int MaxCollectedGen);
extern void HeapMon_MarkRegion (heap_t *heap, ml_val_t *base, Word_t szB, aid_t aid);
extern void HeapMon_MarkFromSp (heap_t *heap, ml_val_t *base, Word_t szB);

#else

/* Macros to nullify calls to the heap monitor routines. */
#define HeapMon_StartGC(A,B)
#define HeapMon_UpdateHeap(A,B)
#define HeapMon_MarkRegion(A,B,C,D)
#define HeapMon_MarkFromSp(A,B,C)

#endif

#endif /* !_HEAP_MONITOR_ */

