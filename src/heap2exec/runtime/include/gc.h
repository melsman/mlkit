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

typedef struct {	/* object information */
    Word_t	*objStart;  /* the address of the beginning of the object's */
			    /* storage (including the descriptor) */
    Addr_t	sizeB;	    /* the size of the storage (including descriptor) */
    int		gen;	    /* the object's generation (-1 for unboxed) */
    int		kind;	    /* the object's kind (i.e., arena index). Note that */
			    /* the isBig field effects the meaning of this. */
    bool_t	isExternal; /* true, if the object is a runtime system */
			    /* reference. */
    bool_t	isSpecial;  /* true, if this is a special object (e.g., weak */
			    /* pointer). */
    bool_t	isBig;	    /* true, if the object is big */
    bool_t	isEmbedded; /* true, if the object is a pointer to a literal */
			    /* embedded in a big object. */
    bool_t	isAtomic;   /* true, if the object does not contain pointers */
} obj_info_t;

extern obj_info_t GetObjInfo (ml_val_t obj);

char *BO_AddrToCodeObjTag (Word_t pc, char *buf, int bufSz);

#ifdef HEAP_MONITOR
extern status_t HeapMon_Init (heap_t *heap);
#else
#define HeapMon_Init(A)
#endif

#ifdef GC_STATS
extern void ClearGCStats (heap_t *heap);
#endif

#endif /* !_GC_ */
