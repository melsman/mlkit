/* gc.h
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *
 * The external interface to the garbage collector.
 *
 */

#ifndef _GC_
#define _GC_

#ifndef _ML_BASE_
#include "ml-base.h"
#endif

/* typedef struct heap heap_t; */	/* from ml-base.h */

extern void InitHeap (ml_state_t *msp, bool_t isBoot, heap_params_t *params);
extern void InvokeGC (ml_state_t *msp, int level);
extern void InvokeGCWithRoots (ml_state_t *msp, int level, ...);
extern bool_t NeedGC (ml_state_t *msp, Word_t nbytes);

extern int GetObjGen (ml_val_t obj);
extern ml_val_t RecordConcat (ml_state_t *msp, ml_val_t r1, ml_val_t r2);

char *BO_AddrToCodeObjTag (Word_t pc);

#ifdef HEAP_MONITOR
extern status_t HeapMon_Init (heap_t *heap);
#else
#define HeapMon_Init(A)
#endif

#ifdef GC_STATS
extern void ClearGCStats (heap_t *heap);
#endif

#endif /* !_GC_ */
