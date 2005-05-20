#ifndef HEAP_CACHE_H
#define HEAP_CACHE_H

/*
 * Checkpointing execution of library code
 */

#include "Region.h"
#include "Stack.h"

// Pages are layed out in continuous memory, where each page 
// (ALLOCATABLE_WORDS_IN_REGION_PAGE words) is prefixed with a 
// pointer to the origin region page.

typedef struct regionCopy {
  int *a; // allocation pointer
  int *b; // border pointer
  Ro *r;  // origin region
  int pages[0];
} RegionCopy;
  
#define HSTAT_UNINITIALIZED  0
#define HSTAT_DIRTY          1
#define HSTAT_CLEAN          2

// In the case that the global exception handler is triggered, the
// bottom of the stack is destroyed by the raise instruction; therefore
// we copy this part of the stack in a separate block in the heap, which 
// allows the stack to be reestablished.
#define LOWSTACK_COPY_SZ     6

// Maximum number of allocated heaps (stacks and initial region pages)
// in the heap pool - important only for the multi-threaded SMLserver.
// The effect of using a heap from the heap pool is that execution of
// library code is cached. To enable execution of library code for
// every request, set MAX_HEAP_POOL_SZ to 0.
#define MAX_HEAP_POOL_SZ     6

typedef struct heap {
  int heapid;               // unique heap id
  int status;               // heap status
  RegionCopy *r0copy;       // rtype top
  RegionCopy *r2copy;       // rtype pair
  RegionCopy *r3copy;       // rtype string
  RegionCopy *r4copy;       // rtype array
  RegionCopy *r5copy;       // rtype ref
  RegionCopy *r6copy;       // rtype triple
  int *sp;                  // stack pointer
  int *exnPtr;
  unsigned long exnCnt;
  int lowStack[LOWSTACK_COPY_SZ]; // copy of global exception handler, etc.
  int ds[STACK_SIZE_INIT];  // start of data-space
                            //   followed by stack
} Heap;

// [getHeap()] returns a heap h from the pool of heaps with the status
// set to either HSTAT_UNINITIALIZED or HSTAT_CLEAN. In the latter
// case, the stack pointer h->sp and the dataspace counter &(h->ds)
// can be extracted and used for interpretation; all what remains is
// to interpret the leaf bytecode. In the former case, library code
// need first be executed, after which, the initializeHeap() function
// should be called.
Heap* getHeap(void);

// [touchHeap(h)] changes the status of the heap h to HSTAT_DIRTY.
// Requires the status to be HSTAT_CLEAN. 
void touchHeap(Heap *h);

// [releaseHeap(h)] restores the heap from the heap copy information
// and gives back the heap h to the pool of heaps. Requires the heap
// status to be HSTAT_DIRTY.
void releaseHeap(Heap *h);

// [initializeHeap(h,sp,exnPtr,exnCnt)] This function should be
// called after library code is executed, but before leaf bytecode is
// executed. The function changes the status of the heap to
// HSTAT_CLEAN. It requires the heap status to be HSTAT_UNINITIALIZED.
void initializeHeap(Heap *h, int *sp, int *exnPtr, unsigned long exnCnt);

// [deleteHeap(h)] deletes the heap by freeing it. Also frees region
// pages in the regions in the heap.
void deleteHeap(Heap *h);

// [clearHeapCache()] deletes all heaps in the pool of heaps. Assumes 
// that no client has a handle to a heap.
void clearHeapCache();

#endif
