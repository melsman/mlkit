/*----------------------------------------------------------------*
 *                         Regions                                *
 *----------------------------------------------------------------*/
#ifndef __REGION__
#define __REGION__

/*
Overview
--------

This module defines the runtime representation of regions. 

There are two types of regions: {\em finite} and {\em infinite}.
A region is finite if its (finite) size has been found at compile
time and to which at most one object will ever be written. 
Otherwise it is infinite.

The runtime representation of a region depends on whether the region
is finite or infinite.

We describe each of the four possibilities in turn.

(a) Finite region of size n bytes (n%4==0) -- meaning that
    every object that may be stored in the region has size
    at most n bytes: the region is n/4 words on the runtime stack
(b) Infinite region -- meaning that the region can contain objects
    of different sizes. The region is represented by a
    {\em region descriptor} on the runtime stack. The region descriptor
    points to the beginning and the end of a linked list of
    fixed size region pages (see below). 

A {\em region page} consists of a header and an array of words that
can be used for allocation.  The header takes up
HEADER_WORDS_IN_REGION_PAGE words, while the number of words
that can be allocated is ALLOCATABLE_WORDS_IN_REGION_PAGE.
Thus, a region page takes up
HEADER_WORDS_IN_REGION_PAGE + ALLOCATABLE_WORDS_IN_REGION_PAGE
words in all.
*/


#define ALLOCATABLE_WORDS_IN_REGION_PAGE 63 
// A region page is 256 bytes.

// Region pages are word aligned. Make sure that it's ok for regionpages containing double's.
typedef union regionpage {
    union regionpage* n;                         /* NULL or pointer to next page. */
    ULong i[ALLOCATABLE_WORDS_IN_REGION_PAGE];   /* Space for data*/
} Regionpage;

#define HEADER_WORDS_IN_REGION_PAGE 1 

/*
Free region pages are kept in a free list. When the free list becomes empty and
more space is required, the runtime system calls the Palm operating system
in order to get space for a number (here 10) fresh region pages:
*/
#define NUM_REG_PAGES_ALLOC_BY_SBRK 30

/* 
Region descriptors
------------------
regiondesc is the type of region descriptors. Region descriptors are kept on
the runtime stack and are linked together so that one can traverse the stack
of regions (for popping of regions when exceptions are raised) 
*/

typedef struct regiondesc {
  struct regiondesc* p;    /* Pointer to previous region descriptor. It has to be at 
                              the bottom of the structure */
  ULong* a;                /* Pointer to first unused word in the newest region page
                              of the region. */
  ULong* b;                /* Pointer to the border of the newest region page, defined as the address
                              of the first word to follow the region page. One maintains
                              the invariant a<=b;  a=b means the region page is full.*/
  Regionpage* fp;          /* Pointer to the oldest (first allocated) page of the region. 
                              The beginning of the newest page of the region can be calculated
                              as a fixed offset from b. Thus the region descriptor gives
                              direct access to both the first and the last region page
                              of the region. This makes it possible to de-allocate the
                              entire region in constant time, by appending it to the free list.*/

} Regiondesc;
#define sizeRd                (sizeof(Regiondesc)/4) /* Size of region descriptor in words */
#define freeInRegion(rAddr)   (rAddr->b - rAddr->a)  /* Returns freespace in words. */

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
extern ULong bytes_alloc;
extern Regionpage* freelist;
extern Regiondesc* topRegion;

ULong *alloc_region(Regiondesc *rdAddr);
ULong *dealloc_region();
ULong *alloc (ULong rdAddr, int n);
ULong reset_region(ULong rdAddr);
void dealloc_regions_until(ULong rdAddr);
void init_runtime_system();

#endif /*__REGION__*/




