/*----------------------------------------------------------------*
 *                         Regions                                *
 *----------------------------------------------------------------*/
#ifndef REGION_H
#define REGION_H

#include <stdint.h>
#include "Flags.h"

/*
Overview
--------

This module defines the runtime representation of regions.

There are two types of regions: {\em finite} and {\em infinite}.
A region is finite if its (finite) size has been found at compile
time and to which at most one object will ever be written.
Otherwise it is infinite.

The runtime representation of a region depends on
  (a) whether the region is finite or infinite;
  (b) whether profiling is turned on or not.

We describe each of the four possibilities in turn.

(a) Finite region of size n bytes (n%4==0) -- meaning that
    every object that may be stored in the region has size
    at most n bytes:
    (i)  without profiling, the region is n/4 words on the
         runtime stack;
    (ii) with profiling, the region is represented by first
         pushing a region descriptor (see below) on the stack,
         then pushing an object descriptor (see below) on the stack and
         then reserving space for the object; the region descriptors
         of finite regions are linked together which the profiler
         can traverse.
(b) Infinite region -- meaning that the region can contain objects
    of different sizes.
    (i)  without profiling, the region is represented by a
         {\em region descriptor} on the stack. The region descriptor
         points to the beginning and the end of a linked list of
         fixed size region pages (see below).
    (ii) with profiling, the representation is the same as without
         profiling, except that the region descriptor contains more
         fields for profiling statistics.

A {\em region page} consists of a header and an array of words that
can be used for allocation.  The header takes up
HEADER_WORDS_IN_REGION_PAGE words, while the number of words
that can be allocated is ALLOCATABLE_WORDS_IN_REGION_PAGE.
Thus, a region page takes up
HEADER_WORDS_IN_REGION_PAGE + ALLOCATABLE_WORDS_IN_REGION_PAGE
words in all.

*/

// Uncomment the following line to enable region page statistics (for SMLserver)
//#define REGION_PAGE_STAT 1
#ifdef REGION_PAGE_STAT
typedef struct regionPageMapHashList {
  unsigned long n;                               /* reuse number */
  uintptr_t addr;                            /* address of region page */
  struct regionPageMapHashList * next; /* next hashed element */
} RegionPageMapHashList;

typedef RegionPageMapHashList* RegionPageMap;

/* Size of region page map hash table in entries -- (2^n-1) */
#define REGION_PAGE_MAP_HASH_TABLE_SIZE 511
#define hashRegionPageIndex(addr) ((addr) % REGION_PAGE_MAP_HASH_TABLE_SIZE)
RegionPageMap* regionPageMapNew(void);
extern RegionPageMap* rpMap;
#endif /* REGION_PAGE_STAT */

/*
 * Number of words that can be allocated in each regionpage and number
 * of words in the header part of each region page.
 *
 * HEADER_WORDS_IN_REGION_PAGE + ALLOCATABLE_WORDS_IN_REGION_PAGE must
 * be one 1Kb - used by GC.
 */

#ifdef ENABLE_GEN_GC
#define HEADER_WORDS_IN_REGION_PAGE 3
#if defined(__LP64__) || (__WORDSIZE == 64)
#define ALLOCATABLE_WORDS_IN_REGION_PAGE 125
#else
#define ALLOCATABLE_WORDS_IN_REGION_PAGE 253
#endif
#else /* not(ENABLE_GEN_GC) */
#define HEADER_WORDS_IN_REGION_PAGE 2
#if defined(__LP64__) || (__WORDSIZE == 64)
#define ALLOCATABLE_WORDS_IN_REGION_PAGE 126
#else
#define ALLOCATABLE_WORDS_IN_REGION_PAGE 254
#endif
#endif /* ENABLE_GEN_GC */

typedef struct rp {
  struct rp *n;                   /* NULL or pointer to next page. */
  struct gen *gen;                /* Pointer back to generation. Used by GC. */
#ifdef ENABLE_GEN_GC
  uintptr_t *colorPtr;         /* Color pointer used by generational GC */
#endif /* ENABLE_GEN_GC */
  uintptr_t i[ALLOCATABLE_WORDS_IN_REGION_PAGE];  /* space for data*/
} Rp;

/* #if defined(__LP64__) || (__WORDSIZE == 64) */
/* #define SIZE_REGION_PAGE 0x800 */
/* #else */
/* #define SIZE_REGION_PAGE 0x400 */
/* #endif */

#define is_rp_aligned(rp)  (((rp) & (sizeof(Rp)-1)) == 0)

/* Free pages are kept in a free list. When the free list becomes
 * empty and more space is required, the runtime system calls the
 * operating system function malloc in order to get space for a number
 * (here 30) of fresh region pages: */

/* Size of allocated space in each SBRK-call. */
#define BYTES_ALLOC_BY_SBRK REGION_PAGE_BAG_SIZE*sizeof(Rp)

/* When garbage collection is enabled, a single bit in a region page
 * descriptor specifies if the page is part of to-space during garbage
 * collection. At points between garbage collections, no region page
 * descriptor has the bit set; given a region page descriptor, the
 * to-space bit is the least significant bit in the next-pointer in
 * the region page descriptor. */

#ifdef ENABLE_GC
#define clear_tospace_bit(p)  (Rp*)((uintptr_t)(p) & (UINTPTR_MAX ^ 0x1))
// #define clear_tospace_bit(p)  (Rp*)((unsigned long)(p) & 0xFFFFFFFE)
#define set_tospace_bit(p)    (Rp*)((uintptr_t)(p) | 0x1)
#define is_tospace_bit(p)     ((uintptr_t)(p) & 0x1)
#else
#define clear_tospace_bit(p)  (p)
#endif

/* Region large objects idea: Modify the region based memory model so
 * that each ``infinite region'' (i.e., a region for which the size of
 * the region is not determined statically) contains a list of objects
 * allocated using the C library function `malloc'. When a region is
 * deallocated or reset, the list of malloced objects in the region
 * are freed using the C library function `free'. The major reason for
 * large object support is to gain better indexing properties for
 * arrays and vectors. Without support for large objects, arrays and
 * vectors must be split up in chunks. With strings implemented as
 * linked list of pages, indexing in large character arrays takes time
 * which is linear in the index parameter and with word-vectors and
 * word-arrays being implemented as a tree structure, indexing in
 * word-vectors and word-arrays take time which is logarithmic in the
 * index parameter.  -- mael 2001-09-13 */

/* For tag-free garbage collection of pairs, triples, and refs, we
 * make sure that large objects are aligned on 1K boundaries, which
 * makes it possible to determine if a pointer points into the stack,
 * constants in data space, a region in from-space, or a region in
 * to-space. The orig pointer points back to the memory allocated by
 * malloc (which holds the large object). */

typedef struct lobjs {
  struct lobjs* next;     // pointer to next large object or NULL
#ifdef ENABLE_GC
  void* orig;             // pointer to memory allocated by malloc - for freeing
#endif
#ifdef KAM
  size_t sizeOfLobj;      // size of this object
#endif
  uintptr_t value;     // a large object; inlined to avoid pointer-indirection
} Lobjs;

/* When garbage collection is enabled, a bit in a large object
 * descriptor specifies that the object is indeed a large object. The
 * bit helps the garbage collector determine, given a pointer p to an
 * object, whether p points to a tag-free pair. */

#ifdef ENABLE_GC
#define clear_lobj_bit(p)     (Lobjs*)((uintptr_t)(p) & (UINTPTR_MAX ^ 0x2))
// #define clear_lobj_bit(p)     (Lobjs*)((unsigned long)(p) & 0xFFFFFFFD)
#define set_lobj_bit(p)       (Lobjs*)((uintptr_t)(p) | 0x2)
#define is_lobj_bit(p)        ((uintptr_t)(p) & 0x2)
#else
#define clear_lobj_bit(p)     (p)
#define set_lobj_bit(p)       (p)
#endif /* ENABLE_GC */

/* By introducing generational garbage collection we need two region
   page lists in each region descriptor. We therefore define a
   sub-structure called Gen (for generation) containing the three
   pointers controlling the allocation into a generation: fp, a and
   b. */

typedef struct gen {
  uintptr_t * a;  /* Pointer to first unused word in the newest region
                     page of the region. */
  uintptr_t * b;  /* Pointer to the border of the newest region page,
                     defined as the address of the first word to
                     follow the region page. One maintains the
                     invariant a<=b; a=b means the region page is
                     full. */
  Rp *fp;   /* Pointer to the oldest (first allocated) page of the
               region.  The beginning of the newest page of the region
               can be calculated as a fixed offset from b. Thus the
               region descriptor gives direct access to both the first
               and the last region page of the region. This makes it
               possible to deallocate the entire region in constant
               time, by appending it to the free list. */
} Gen;


/*
Region descriptors
------------------
ro is the type of region descriptors. Region descriptors are kept on
the stack and are linked together so that one can traverse the stack
of regions (for profiling and for popping of regions when exceptions
are raised) */

/* Important: don't mess with Ro unless you also redefine the constants below. */
#define offsetG0InRo 0
#ifdef ENABLE_GEN_GC
#define offsetG1InRo (sizeof(Gen))  /* bytes */
#endif
typedef struct ro {
  Gen g0;              /* g0 is the only generation when ordinary GC is used. g0
                          is the youngest generation when using generational GC. */

  #ifdef ENABLE_GEN_GC
  Gen g1;              /* g1 is the old generation. */
  #endif

  struct ro * p;       /* Pointer to previous region descriptor. It has to be at
                          the bottom of the structure */

  /* here are the extra fields that are used when profiling is turned on: */
  #ifdef PROFILING
  size_t allocNow;     /* Words allocated in region (excl. profiling data). */
  size_t allocProfNow; /* Words allocated in region for profiling data. */
  size_t regionId;     /* Id on region. */
  #endif

  Lobjs *lobjs;     // large objects: a list of malloced memory in each region

} Ro;

typedef Ro* Region;

#define sizeRo (sizeof(Ro)/(sizeof(long*))) /* size of region descriptor in words */
#define sizeRoProf (3)        /* We use three words extra when profiling. */

#ifdef ENABLE_GEN_GC
#define MIN_NO_OF_PAGES_IN_REGION 2
#else
#define MIN_NO_OF_PAGES_IN_REGION 1
#endif /* ENABLE_GEN_GC */

#define freeInRegion(rAddr)   (rAddr->g0.b - rAddr->g0.a) /* Returns freespace in words. */

#define descRo_a(rAddr,w) (rAddr->g0.a = rAddr->g0.a - w) /* Used in IO.inputStream */


// When GC is enabled, bits in the region descriptor (in the r->g0.fp pointer)
// are used to tell the type of values in the region, in the
// case that the values are untagged. Because region pages are aligned
// on 1k boundaries, plenty of bits are available in the r->g0.fp pointer.
// We use the three least significant bits:
//
//     000    (hex 0x0)   ordinary tagged values
//     001    (hex 0x1)   pairs
//     010    (hex 0x2)   arrays   (value is tagged, but the region type
//                                  is needed by generational collector)
//     011    (hex 0x3)   refs
//     111    (hex 0x7)   triples

// To make Generational GC possible we use two more bits to encode
//   (1) the status SOME or NONE saying whether the generation is
//       on the scan stack or not
//   (2) the generation (being either 0 or 1). This info is used
//       to calculate the address of the region descriptor given
//       a pointer to the generation (either g0 or g1) in the
//       region descriptor.
//    X1XXX status SOME saying that the generation is on the scan stack
//    X0XXX status NONE saying that the generation is not on the scan stack
//    0XXXX this is generation 0  (young generation)
//    1XXXX this is generation 1  (old generation)
// Notice, that the generation g0 is always used no matter what mode
// the compiler is in (no gc, gc or gen gc). The generation g1 is only
// used when generational gc is enabled. It is thus always possible to
// write r->g0, whereas r->g1 makes sense only when generational gc is
// enabled.
// We do not explicitly set the generation 0 bit when allocating a
// region because the bit is 0 by default, that is, set_gen_0 is not
// used in Region.c

#if defined(ENABLE_GC) || defined(PROFILING)
#define RTYPE_PAIR          0x1
#define RTYPE_ARRAY         0x2
#define RTYPE_REF           0x3
#define RTYPE_TRIPLE        0x7
#define GENERATION_STATUS   0x8
#define GENERATION          0x10
#define rtype(gen)           ((uintptr_t)((gen).fp) & 0x07)
#define all_marks_fp(gen)     ((uintptr_t)((gen).fp) & 0x1F)
#define gen_status(gen)      ((uintptr_t)((gen).fp) & GENERATION_STATUS)
#define generation(gen)      ((uintptr_t)((gen).fp) & GENERATION)
#define clear_fp(fp)        ((Rp*)((uintptr_t)(fp) & (UINTPTR_MAX ^ 0x1F)))
// #define clear_fp(fp)        ((Rp*)((unsigned long)(fp) & 0xFFFFFFE0))
#define is_pairregion(gen)   (rtype(gen) == RTYPE_PAIR)
#define is_arrayregion(gen)  (rtype(gen) == RTYPE_ARRAY)
#define is_refregion(gen)    (rtype(gen) == RTYPE_REF)
#define is_tripleregion(gen) (rtype(gen) == RTYPE_TRIPLE)
#define set_fp(gen,rt)       ((gen).fp = (Rp*)(((uintptr_t)((gen).fp)) | (rt)))
#define set_pairregion(gen)  (set_fp((gen),RTYPE_PAIR))
#define set_arrayregion(gen) (set_fp((gen),RTYPE_ARRAY))
#define set_refregion(gen)   (set_fp((gen),RTYPE_REF))
#define set_tripleregion(gen) (set_fp((gen),RTYPE_TRIPLE))
#define set_gen_status_SOME(gen) (set_fp((gen),GENERATION_STATUS))
#define set_gen_status_NONE(gen) ((gen).fp = (Rp*)(((uintptr_t)((gen).fp)) & (UINTPTR_MAX ^ 0x8)))
// #define set_gen_status_NONE(gen) ((gen).fp = (Rp*)(((unsigned long)((gen).fp)) & 0xFFFFFFF7))
#define is_gen_status_NONE(gen)  (gen_status(gen) == 0)
#define set_gen_0(gen)           ((gen).fp = (Rp*)(((uintptr_t)((gen).fp)) & (UINTPTR_MAX ^ 0x10)))
// #define set_gen_0(gen)           ((gen).fp = (Rp*)(((unsigned long)((gen).fp)) & 0xFFFFFFEF))
#define set_gen_1(gen)           (set_fp((gen),GENERATION))
#define is_gen_1(gen)            (generation(gen) == GENERATION)
#else
#define is_gen_1(gen)            (0) /* Only g0 exists if no GC enabled */
#define clear_fp(fp)     (fp)
#endif /*ENABLE_GC*/

#ifdef ENABLE_GEN_GC
#define get_ro_from_gen(gen)    ( (is_gen_1(gen)) ? ( (Ro*)(((uintptr_t)(&(gen)))-offsetG1InRo) ) : ( (Ro*)(((uintptr_t)(&(gen)))-offsetG0InRo) ) )
#else
#define get_ro_from_gen(gen)    ( (Ro*)(((uintptr_t)(&(gen)))-offsetG0InRo) )
#endif /* ENABLE_GEN_GC */

/*
Region polymorphism
-------------------
Regions can be passed to functions at runtime. The machine value that represents
a region in this situation is a 32 bit word. The least significant bit is 1
iff the region is infinite. The second least significant bit is 1 iff stores
into the region should be preceded by emptying the region of values before
storing the new value (this is called storing a value at the {\em bottom}
of the region and is useful for, among other things, tail recursion).

*/

/* Operations on the two least significant   */
/* bits in a regionpointer.                  */
/* C ~ 1100, D ~ 1101, E ~ 1110 og F ~ 1111. */
#define setInfiniteBit(x)   ((x) | 0x1)
#define clearInfiniteBit(x) ((x) & (UINTPTR_MAX ^ 0x1))
// #define clearInfiniteBit(x) ((x) & 0xFFFFFFFE)
#define setAtbotBit(x)      ((x) | 0x2)
#define clearAtbotBit(x)    ((x) & (UINTPTR_MAX ^ 0x2))
// #define clearAtbotBit(x)    ((x) & 0xFFFFFFFD)
#define setStatusBits(x)    ((x) | 0x3)
#define clearStatusBits(x)  ((Region)(((uintptr_t)(x)) & (UINTPTR_MAX ^ 0x3)))
// #define clearStatusBits(x)  ((Region)(((unsigned long)(x)) & 0xFFFFFFFC))
#define is_inf_and_atbot(x) ((((uintptr_t)(x)) & 0x3)==0x3)
#define is_inf(x)           ((((uintptr_t)(x)) & 0x1)==0x1)
#define is_atbot(x)         ((((uintptr_t)(x)) & 0x2)==0x2)

/*----------------------------------------------------------------*
 * Type of freelist and top-level region                          *
 *                                                                *
 * When the KAM backend is used, we use an indirection to hold    *
 * the top-level region, so as to support multiple threads.       *
 *----------------------------------------------------------------*/

extern Rp * freelist;

#ifdef KAM
#define TOP_REGION   (*topRegionCell)
void free_region_pages(Rp* first, Rp* last);
#else
extern Ro * topRegion;
#define TOP_REGION   topRegion
#endif


/*----------------------------------------------------------------*
 *        Prototypes for external and internal functions.         *
 *----------------------------------------------------------------*/
#ifdef KAM
Region allocateRegion(Region roAddr, Region* topRegionCell);
void deallocateRegion(Region* topRegionCell);
void deallocateRegionsUntil(Region rAdr, Region* topRegionCell);
#else
Region allocateRegion(Region roAddr);
void deallocateRegion();
void deallocateRegionsUntil(Region rAddr);
void deallocateRegionsUntil_X64(Region rAddr);
#endif

uintptr_t *alloc (Region r, size_t n);
uintptr_t *allocGen (Gen *gen, size_t n);
void alloc_new_block(Gen *gen);
void callSbrk();

#ifdef ENABLE_GC_OLD
void callSbrkArg(size_t no_of_region_pages);
#endif

#ifdef ENABLE_GC
Region allocatePairRegion(Region roAddr);
Region allocateArrayRegion(Region roAddr);
Region allocateRefRegion(Region roAddr);
Region allocateTripleRegion(Region roAddr);
#ifdef PROFILING
Region allocPairRegionInfiniteProfiling(Region r, size_t regionId);
Region allocPairRegionInfiniteProfilingMaybeUnTag(Region r, size_t regionId);
Region allocArrayRegionInfiniteProfiling(Region r, size_t regionId);
Region allocArrayRegionInfiniteProfilingMaybeUnTag(Region r, size_t regionId);
Region allocRefRegionInfiniteProfiling(Region r, size_t regionId);
Region allocRefRegionInfiniteProfilingMaybeUnTag(Region r, size_t regionId);
Region allocTripleRegionInfiniteProfiling(Region r, size_t regionId);
Region allocTripleRegionInfiniteProfilingMaybeUnTag(Region r, size_t regionId);
#endif /* PROFILING */
#endif /* ENABLE_GC */

Region resetRegion(Region r);
size_t NoOfPagesInRegion(Region r);
size_t NoOfPagesInGen(Gen* gen);

/*----------------------------------------------------------------*
 *        Declarations to support profiling                       *
 *----------------------------------------------------------------*/
#define notPP 0 /* Also used by GC */
#ifdef PROFILING

/*
Here is the type of region descriptors for finite regions when
profiling is enabled (see item (a)(ii) at the beginning of the file):
*/

typedef struct finiteRegionDesc {
  struct finiteRegionDesc * p;  /* Has to be in the bottom of the descriptor
                                   for deallocation. */
  size_t regionId;                 /* If msb. set then infinite region. (? - mads)*/
} FiniteRegionDesc;
#define sizeFiniteRegionDesc (sizeof(FiniteRegionDesc)/sizeof(long*))

/*
Object descriptors
------------------
When profiling is turned on, every object is prefixed by an
object descriptor, containing the information that is needed
in order to traverse objects in regions and identify allocation
points in the source program. A {\em program point} is an integer
which identifies the point in the source program where a value
is created - the user turns on a flag in the compiler to make
it print programs annotated with their program points.

Every object is stored taking up a multiple of words (not bytes).
This applies irrespective of whether profiling is turned on or not.
*/

typedef struct objectDesc {
  size_t atId;               /* Allocation point. */
  size_t size;               /* Size of object in words. */
} ObjectDesc;
#define sizeObjectDesc (sizeof(ObjectDesc)/(sizeof(long*)))

/*
Profiling is done by scanning the store at regular intervals.
Every such interruption of the normal execution is called
a {\em profile tick}. During a profile tick, the runtime system
scans all the regions accessible from the region stack (which
is one of the reasons why region descriptors are linked together).
The scanning of an infinite region is done by scanning each page
in turn. Scanning of a page starts at the left end and progresses
from object to object (using the size information that prefixes
every object) and it stops when the value 'notPP' follows after
an object:
*/

/*----------------------------------------------------------------------*
 * Extern declarations, mostly of global variables that store profiling *
 * information. See Hallenberg's report for details.
 * ---------------------------------------------------------------------*/
extern unsigned long callsOfDeallocateRegionInf,
                    callsOfDeallocateRegionFin,
                    callsOfAlloc,
                    callsOfResetRegion,
                    callsOfDeallocateRegionsUntil,
                    callsOfAllocateRegionInf,
                    callsOfAllocateRegionFin,
                    callsOfSbrk,
                    maxNoOfPages,
                    noOfPages,
                    allocNowInf,
                    maxAllocInf,
                    allocNowFin,
                    maxAllocFin,
                    allocProfNowInf,
                    maxAllocProfInf,
                    allocProfNowFin,
                    maxAllocProfFin,
                    maxAlloc,
                    regionDescUseInf,
                    maxRegionDescUseInf,
                    regionDescUseProfInf,
                    maxRegionDescUseProfInf,
                    regionDescUseProfFin,
                    maxRegionDescUseProfFin,
                    maxProfStack,
                    allocatedLobjs;

extern FiniteRegionDesc * topFiniteRegion;
// extern uintptr_t size_to_space;

/* Profiling functions. */
Region allocRegionInfiniteProfiling(Region roAddr, size_t regionId);
Region allocRegionInfiniteProfilingMaybeUnTag(Region roAddr, size_t regionId);
void allocRegionFiniteProfiling(FiniteRegionDesc *rdAddr, size_t regionId, size_t size);
void allocRegionFiniteProfilingMaybeUnTag(FiniteRegionDesc *rdAddr, size_t regionId, size_t size);
void deallocRegionFiniteProfiling(void);
uintptr_t *allocProfiling(Region r,size_t n, size_t pPoint);  // used by Table.c
uintptr_t *allocGenProfiling(Gen *gen, size_t n, size_t pPoint);  // used by Table.c
#endif /*Profiling*/

void printTopRegInfo();
size_t size_free_list();
void pp_reg(Region r,  char *str);
void pp_gen(Gen *gen);
void chk_obj_in_gen(Gen *gen, uintptr_t *obj_ptr, char* s);

void free_lobjs(Lobjs* lobjs);

void RegionLocksInit(void);

#endif /*REGION_H*/
