/* heap.h
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * These are the definitions for the heap structure.
 */

#ifndef _HEAP_
#define _HEAP_

#ifndef _ML_BASE_
#include "ml-base.h"
#endif

#ifndef _GC_
#include "gc.h"
#endif

#ifndef _ARENA_ID_
#include "arena-id.h"
#endif

#ifndef _TAGS_
#include "tags.h"
#endif

#ifndef _MEMORY_
typedef struct mem_obj mem_obj_t;
#endif

#ifndef _CARD_MAP_
typedef struct card_map card_map_t;
#endif

#if ((defined(COLLECT_STATS) || defined(GC_STATS)) && (! defined(_CNTR_)))
#include "cntr.h"
#endif

struct heap_params {
    Addr_t	allocSz;	/* the size of the allocation arena */
    int		numGens;
    int		cacheGen;
};

typedef struct repair repair_t;
typedef struct arena arena_t;
typedef struct bigobj_region bigobj_region_t;
typedef struct bigobj_desc bigobj_desc_t;
typedef struct generation gen_t;
/* typedef struct heap heap_t; */ /** defined in ml-base.h **/


/** A heap **
 * A heap consists of an allocation space and one or more generations.
 */
struct heap {
    ml_val_t	    *allocBase;		/* The base address of the allocation arena */
    Addr_t	    allocSzB;		/* The size in bytes of the allocation arena */
    mem_obj_t	    *baseObj;		/* The OS memory object that contains the */
		    			/* BIBOP and allocation arena. */
    int		    numGens;		/* The number of active generations. */
    int		    cacheGen;		/* Cache the from-space for gens 1..cacheGen. */
    int		    numMinorGCs;	/* The number of times the allocation space */
					/* has been collected. */
    gen_t	    *gen[MAX_NUM_GENS]; /* generation #i is gen[i-1] */
    int		    numBORegions;	/* the number of active big-object regions */
    bigobj_region_t *bigRegions;	/* points to the list of big object regions. */
    bigobj_desc_t   *freeBigObjs;	/* points to the header of the free-list */
					/* of big objects. */
    ml_val_t	    *weakList;		/* A list of weak pointers forwarded*/
					/* during GC. */
#if (defined(COLLECT_STATS) || defined(GC_STATS))
    cntr_t	    numAlloc;		/* Keep track of the number of bytes */
					/* allocated and the number copied into */
#ifdef GC_STATS
    cntr_t	    numCopied		/* each arena. */
			[MAX_NUM_GENS][NUM_ARENAS];
#endif
#endif
#ifdef HEAP_MONITOR
    struct monitor  *monitor;		/* The various graphical data structures */
					/* for monitoring the heap. */
#endif
};

#ifdef OLD
/* once we figure out multiple arenas for the MP version, we should
 * be able to go back to the old version of this.
 */
#define HEAP_LIMIT(hp)	\
    (ml_val_t *)((Addr_t)((hp)->allocBase) + (hp)->allocSzB - HEAP_BUF_SZB)
#else
#define HEAP_LIMIT_SIZE(base,size)	\
    (ml_val_t *)((Addr_t)(base) + (size) - HEAP_BUF_SZB)

#define HEAP_LIMIT(hp)	HEAP_LIMIT_SIZE((hp)->allocBase,(hp)->allocSzB)
#endif


/** A generation **/
struct generation {
    heap_t	    *heap;	/* A back pointer to the heap data structure */
    int		    genNum;	/* Which generation this is (1..numGens) */
    int		    numGCs;	/* The number of times this generation has been */
				/* collected. */
    int		    lastPrevGC;	/* The number GCs of the previous (younger) generation */
				/* the last time this generation was collected. */
    int		    ratio;	/* The desired number of collections of the previous */
				/* generation for one collection of this generation */
    arena_t	    *arena[NUM_ARENAS];
    bigobj_desc_t   *bigObjs[NUM_BIGOBJ_KINDS];
    mem_obj_t	    *toObj;	/* The O.S. memory objects that this generation is */
    mem_obj_t	    *fromObj;	/* using for the to-space and from-space */
    mem_obj_t	    *cacheObj;	/* For younger generations, we cache the virtual */
				/* memory of from-space, instead of giving it back. */
    card_map_t	    *dirty;	/* The dirty cards in the array arena of this gen. */
};


/** An arena **/
struct arena {
    aid_t	id;		/* The to-space version of this arena's identifier */
    ml_val_t	*nextw;		/* The next word to allocate in this arena's to-space */
    ml_val_t	*tospBase;	/* the base address and size of to-space. */
    Addr_t	tospSizeB;
    ml_val_t	*tospTop;	/* The top of the to-space (tospBase+tospSizeB). */
    ml_val_t	*sweep_nextw;	/* The next word to sweep in the to-space arena */
    repair_t	*repairList;	/* points to the top of the repair list (for */
				/* blasting out objects).  The repair list grows */
				/* down in to-space. */
    ml_val_t	*frspBase;	/* the base address and size of from-space. */
    Word_t	frspSizeB;
    ml_val_t	*frspTop;	/* The top of the used portion of from-space. */
    ml_val_t	*oldTop;	/* The top of the "older" from-space region. Objects */
				/* below oldTop get promoted, those above don't. */
    arena_t	*nextGen;	/* The arena to promote objects to. */
    bool_t	needsRepair;	/* Set to TRUE when exporting, if the arena had */
				/* external references that require repair */
				/* Heap sizing parameters: */
    Word_t	reqSizeB;	/*   requested minimum size for this arena (this is */
				/*   in addition to the required min. size). */
    Word_t	maxSizeB;	/*   a soft maximum size for this arena. */
};

/* Make to-space into from-space */
#define FLIP_ARENA(ap)	{			\
	arena_t	*__ap = (ap);			\
	__ap->frspBase = __ap->tospBase;	\
	__ap->frspSizeB = __ap->tospSizeB;	\
	__ap->frspTop = __ap->nextw;		\
    }

/* Return true if this arena has an allocated ToSpace */
#define isACTIVE(a)		((a)->tospSizeB > 0)

/* Return true if this arena's ToSpace needs sweeping */
#define NEEDS_SWEEPING(a)	((a)->sweep_nextw < (a)->nextw)

/* Return the amount of free space (in bytes) available in an arena */
#define AVAIL_SPACE(a)		((Addr_t)((a)->tospTop) - (Addr_t)((a)->nextw))

/* Return true if the address addr is an older object in this arena */
#define isOLDER(a,addr)		((addr) < (a)->oldTop)


/** Big object regions **
 *
 * Currently, the only big objects are code objects.
 */

/*#define BIGOBJ_PAGE_SHIFT	12*/ /* 4Kb */
#define BIGOBJ_PAGE_SHIFT	10  /* 1Kb */
#define BIGOBJ_PAGE_SZB		(1 << BIGOBJ_PAGE_SHIFT)
#define MIN_BOREGION_SZB	(128*ONE_K)

struct bigobj_region {	    /* A big-object region header */
    Addr_t	    firstPage;	/* the address of the first page of the region */
    int		    nPages;	/* the number of big-object pages in this region */
    int		    nFree;	/* the number of free pages */
    int		    minGen;	/* the minimum generation of the live objects in */
				/* this region. */
    mem_obj_t	    *memObj;	/* the memory object that this is allocated in */
    bigobj_region_t *next;	/* the next region in the list of regions */
    bigobj_desc_t   *objMap[1]; /* the map from pages to big-object descriptors */
};

/* the size of a big-object region header */
#define BOREGION_HDR_SZB(NPAGES)	\
    (sizeof(bigobj_region_t) + ((NPAGES-1)*sizeof(bigobj_desc_t *)))

/* map an address to a big-object page index */
#define ADDR_TO_BOPAGE(R, ADDR)	\
    (((Addr_t)(ADDR) - (R)->firstPage) >> BIGOBJ_PAGE_SHIFT)

/* map an address to a big-object descriptor */
#define ADDR_TO_BODESC(R, ADDR)	\
    ((R)->objMap[ADDR_TO_BOPAGE(R, ADDR)])


struct bigobj_desc {	    /* A big-object descriptor. */
    Addr_t	    obj;	/* the actual object */
    Addr_t	    sizeB;	/* the size of the object in bytes.  When the object */
				/* is in the free list, this will be a multiple of */
				/* BIGOBJ_PAGE_SZB, otherwise it is the exact size. */
    unsigned char   objc;	/* the object class */
    unsigned char   state;	/* the state of the object */
    unsigned char   gen;	/* the object's generation */
    bigobj_region_t *region;	/* the region this big object is in */
    bigobj_desc_t   *prev;	/* the prev and next links.  The big-object free */
    bigobj_desc_t   *next;	/* list is a doubly linked list; the other lists */
				/* are singly linked lists */
};

/* the rounded size of a big-object */
#define BO_ROUNDED_SZB(BDP)	ROUNDUP((BDP)->sizeB, BIGOBJ_PAGE_SZB)

/* the number of big-object pages occupied by a big-object */
#define BO_NUM_BOPAGES(BDP)	(BO_ROUNDED_SZB(BDP) >> BIGOBJ_PAGE_SHIFT)

/* big-object descriptor states */
#define BO_FREE		0	/* a free big-object */
#define BO_YOUNG	1	/* a young object (i.e., one that has never */
				/* been forwarded in its generation */
#define BO_FORWARD	2	/* a forwarded young object */
#define BO_OLD		3	/* an old object */
#define BO_PROMOTE	4	/* a promoted old object */

#define BO_IS_FROM_SPACE(dp)	(((dp)->state & 0x1) != 0)
#define BO_IS_FREE(dp)		((dp)->state == BO_FREE)

/* remove a descriptor from a doubly linked list */
#define REMOVE_BODESC(dp)		{			\
	bigobj_desc_t	*__dp = (dp), *__p, *__n;		\
	__p = __dp->prev;					\
	__n = __dp->next;					\
	__p->next = __n;					\
	__n->prev = __p;					\
    }

/* add a descriptor to a doubly linked list */
#define ADD_BODESC(hdr, desc)	{				\
	bigobj_desc_t	*__hdr = (hdr), *__dp = (desc);		\
	__dp->next = __hdr->next;				\
	__dp->prev = __hdr;					\
	__hdr->next->prev = __dp;				\
	__hdr->next = __dp;					\
    }


/** operations on forward and back-pointers **/

/* follow a back-pointer to the head of an object */
#define FOLLOW_BACKPTR(P, D)	((ml_val_t *)(P) - GET_LEN(D))

/* follow a forward pointer.  HDR is the object header, P is the pointer to
 * the object.
 * NOTE: we need the two type casts for 32/64 bit systems.
 */
#define FOLLOW_FWDOBJ(HDR, P)	\
    ((ml_val_t *)(((ml_val_t *)(HDR))[0]) + ((ml_val_t *)(P) - (ml_val_t *)(HDR)))

/* follow a pair-space forward pointer (this is tagged as a descriptor). */
#define FOLLOW_FWDPAIR(DESC, HDR, P)	\
    ((ml_val_t *)(((Addr_t)(DESC)) & ~MAJOR_MASK) + ((P) - (HDR)))

/* make a pair-space forward pointer (this is tagged as a descriptor). */
#define MAKE_PAIR_FP(NEW_ADDR)	((ml_val_t)((Addr_t)(NEW_ADDR) | TAG_desc))


/** External GC functions **/
extern void MinorGC (ml_state_t *msp, ml_val_t **roots);
extern void MajorGC (ml_state_t *msp, ml_val_t **roots, int level);
extern int Flip (heap_t *heap, int min_gc_level);
extern status_t NewGeneration (gen_t *gen);
extern void FreeGeneration (heap_t *heap, int g);
extern void NewDirtyVector (gen_t *gen);
extern void MarkRegion (bibop_t bibop, ml_val_t *base, Word_t sizeB, aid_t id);
extern void ScanWeakPtrs (heap_t *heap);

extern bigobj_desc_t *BO_AllocRegion (heap_t *heap, Addr_t szB);
extern bigobj_desc_t *BO_Alloc (heap_t *heap, int gen, Addr_t objSzB);
extern void BO_Free (heap_t *heap, bigobj_desc_t *desc);
extern bigobj_desc_t *BO_GetDesc (ml_val_t addr);
extern char *BO_GetCodeObjTag (bigobj_desc_t *bdp, char *buf, int bufSz);

#ifdef BO_DEBUG
extern void PrintRegionMap (bigobj_region_t *r);
#endif
#ifdef CHECK_GC
extern void CheckHeap (heap_t *heap, int maxSweptGen);
#endif

#endif /* !_HEAP_ */
