/*----------------------------------------------------------------*
 *                         Regions                                *
 *----------------------------------------------------------------*/
#ifndef REGION
#define REGION

/*----------------------------------------------------------------*
 * Include files                                                  *
 * Compiling: cc -Aa -c Region.c                                  *
 *----------------------------------------------------------------*/
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

The Danish word 'klump' means 'chunk', in this case:  'region page'.
*/

#define ALLOCATABLE_WORDS_IN_REGION_PAGE 200 
/*Number of words that can be allocated in each regionpage.*/
/*If you change this, remember also to change src/Compiler/Kam/CConst.sml.*/

typedef union klump {
  struct {
    union klump * n;                   /* NULL or pointer to next page. */
    int           dummy;               /* to ensure double alignment    */
    int           i[ALLOCATABLE_WORDS_IN_REGION_PAGE]; /* space for data*/
  } k;
  double x; /* The union ensures alignment of region pages on double aligned
               addresses (i.e., addresses that are divisible by 8). */
} Klump;



#define HEADER_WORDS_IN_REGION_PAGE 2 
/*Number of words in the header part of a region page.*/
/*If you change this, remember also to change src/Compiler/Kam/CConst.sml.*/


/*
Free pages are kept in a free list. When the free list becomes empty and
more space is requiredd, the runtime system calls the operating system
function malloc in order to get space for a number (here 30) fresh
region pages:
*/

#define BYTES_ALLOC_BY_SBRK 30*sizeof(Klump)  /* Size of allocated space in each SBRK-call. */


/*if you change any of the above declarations, remember to also change
 ---at least---Compiler/Hppa/KbpToHpPa.*/

/* 
Region descriptors
------------------

ro is the type of region descriptors. Region descriptors are kept on
the stack and are linked together so that one can traverse the stack
of regions (for profiling and for popping of regions when exceptions
are raised) 
*/

typedef struct ro {
  struct ro * p;       /* Pointer to previous region descriptor. It has to be at 
                          the bottom of the structure */
  int * a;             /* Pointer to first unused word in the newest region page
                          of the region. */
  int * b;             /* Pointer to the border of the newest region page, defined as the address
                          of the first word to follow the region page. One maintains
                          the invariant a<=b;  a=b means the region page is full.*/
  Klump *fp;           /* Pointer to the oldest (first allocated) page of the region. 
                          The beginning of the newest page of the region can be calculated
                          as a fixed offset from b. Thus the region descriptor gives
                          direct access to both the first and the last region page
                          of the region. This makes it possible to de-allocated the
                          entire region in constant time, by appending it to the free list.*/

  /* here are the extra fields that are used when profiling is turned on: */
  #ifdef PROFILING
  unsigned int allocNow;     /* Number of words allocated in region (excl. profiling data). */
  unsigned int allocProfNow; /* Number of words allocated in region for profiling data. */
  unsigned int regionId;     /* Id on region. */
  #endif

} Ro;
#define sizeRo (sizeof(Ro)/4) /* size of region descriptor in words */
#define sizeRoProf (3)        /* We use three words extra when profiling. */

#define freeInRegion(rAddr)   (rAddr->b - rAddr->a) /* Returns freespace in words. */


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
#define setInfiniteBit(x)   ((x) | 0x00000001)
#define clearInfiniteBit(x) ((x) & 0xFFFFFFFE)
#define setAtbotBit(x)      ((x) | 0x00000002)
#define clearAtbotBit(x)    ((x) & 0xFFFFFFFD)
#define setStatusBits(x)    ((x) | 0x00000003)
#define clearStatusBits(x)  ((x) & 0xFFFFFFFC)
#define is_inf_and_atbot(x) (((x) & 0x00000003)==0x00000003)

/*----------------------------------------------------------------*
 *        Prototypes for external and internal functions.         *
 *----------------------------------------------------------------*/
int *allocateRegion(Ro *roAddr);
int *deallocateRegion();
int *alloc (int rAdr, int n);
void alloc_new_block(Ro *rp);     /* inlining of alloc */
void callSbrk();
void resetRegion(int rAdr);
void deallocateRegionsUntil(int rAdr);

/*----------------------------------------------------------------*
 *        Declarations to support profiling                       *
 *----------------------------------------------------------------*/
#ifdef PROFILING

/* 
Here is the type of region descriptors for finite regions when
profiling is enabled (see item (a)(ii) at the beginning of the file):
*/

typedef struct finiteRegionDesc {
  struct finiteRegionDesc * p;  /* Has to be in the bottom of the descriptor 
                                   for deallocation. */
  int regionId;                 /* If msb. set then infinite region. (? - mads)*/
} FiniteRegionDesc;
#define sizeFiniteRegionDesc (sizeof(FiniteRegionDesc)/4)

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

CompLamb.sml (search after storePrgPointProfiling) depends on the 
position of atId in an object descriptor; ME 1998-09-09
*/

typedef struct objectDesc {
  int atId;               /* Allocation point. */
  int size;               /* Size of object in bytes. */
} ObjectDesc;
#define sizeObjectDesc (sizeof(ObjectDesc)/4)

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

#define notPP 0

#define dummyDouble 42424242 /* Sorry, I have no idea what this
                                is (mads). */

/*----------------------------------------------------------------------*
 * Extern declarations, mostly of global variables that store profiling *
 * information. See Hallenberg's report for details.
 * ---------------------------------------------------------------------*/
extern unsigned int callsOfDeallocateRegionInf,
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
                    maxProfStack;

extern Ro * topRegion;
extern FiniteRegionDesc * topFiniteRegion;


/* Profiling functions. */
int *allocRegionInfiniteProfiling(Ro *roAddr, unsigned int regionId);
void allocRegionFiniteProfiling(FiniteRegionDesc *rdAddr, unsigned int regionId, int size);
int *deallocRegionFiniteProfiling(void);
int *allocProfiling(int rAddr,int n, int pPoint);
void storePrgPointProfiling(int pPoint, int *objPtr);
int *updateSizeForDoublesProfiling(int size, int *doublePtr);
#endif /*Profiling*/

#endif /*REGION*/




