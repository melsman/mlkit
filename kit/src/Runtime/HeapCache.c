#include "HeapCache.h"
#include "Region.h"
#include "Runtime.h"

/*
 * static function declarations
 */

// [newHeap()] returns an uninitialized heap - with status 
// HSTAT_UNINITIALIZED.
static Heap* newHeap(void);

// [restoreHeap(h)] restores the heap from the heap copy information.
// Changes the heap status to HSTAT_CLEAN. Requires the heap status to
// be HSTAT_DIRTY. 
static void restoreHeap(Heap *h);

// [pagesInRegion(r)] returns the number of pages associated with r.
static int pagesInRegion(Ro *r);

// [copyRegion(r)] copies the content of the region r into a malloced
// data structure containing all pages from the region and region
// descriptor information.
static RegionCopy* copyRegion(Ro *r);

// [restoreRegion(rc)] restores the region rc->r from the region copy rc
// by copying back the original region page contents into the first
// region pages in the region. The function frees the remaining pages
// of the region. Returns 0 on success and -1 on error.
static int restoreRegion(RegionCopy *rc);


static int heapid_counter = 0;

#ifdef THREADS
#include "/usr/share/aolserver/include/ns.h"
extern Ns_Mutex stackPoolMutex;
#define HEAP_POOL_MUTEX_LOCK     Ns_LockMutex(&stackPoolMutex);
#define HEAP_POOL_MUTEX_UNLOCK   Ns_UnlockMutex(&stackPoolMutex);
#else
#define HEAP_POOL_MUTEX_LOCK
#define HEAP_POOL_MUTEX_UNLOCK
#endif

static Heap* heapPool[MAX_HEAP_POOL_SZ];
static int heapPoolIndex = 0;

// Invariant: if heapPoolIndex == 0 then there are no heaps in the
// heapPool to choose from; otherwise, the heapPool contains a heap
// we can use (index heapPoolIndex). Each heap in the pool has status
// HSTAT_CLEAN.

// [pagesInRegion(r)] returns the number of pages associated with r.
static int pagesInRegion(Ro *r)
{
  Klump *p;
  int n = 0;
  for ( p = r->fp ; p ; p = p->n )
    n++;
  return n;
}

static RegionCopy* copyRegion(Ro *r)
{
  int np, bytes, *q;
  Klump *p;
  RegionCopy *rc;

  if ( r->lobjs ) 
    {
      #ifdef THREADS
      Ns_Log(Error, "Large object constructed during initialization of library code");
      #endif
      die ("copyRegion: copying of large objects not supported");
    }
  // printf("entering copyRegion r = %x\n", r);

  np = pagesInRegion(r);

  // printf("%d pages\n", np);

  bytes = sizeof(RegionCopy) + 4 /*for final null-pointer*/ 
    + np * (4 * (ALLOCATABLE_WORDS_IN_REGION_PAGE + 1));
  rc = (RegionCopy*)malloc(bytes);
  
  rc->r = r;     // not really necessary
  rc->a = r->a;
  rc->b = r->b;

  q = rc->pages;
  for ( p = r->fp ; p ; p = p->n )
    {
      int i;
      (Klump*)(*q++) = p;
      for ( i = 0; i < ALLOCATABLE_WORDS_IN_REGION_PAGE ; i++, q++ )
	*q = p->i[i];
    }
  (Klump*)(*q) = 0;
  return rc;
}

static int restoreRegion(RegionCopy *rc)
{
  Klump *p;
  int i = 0;
  p = (Klump*)(rc->pages[0]);

  while ( (Klump*)(rc->pages[i]) )
    {
      int j = 0;
      p = (Klump*)(rc->pages[i++]);
      while ( j < ALLOCATABLE_WORDS_IN_REGION_PAGE )
	p->i[j++] = rc->pages[i++];
    }
  free_region_pages(p->n,((Klump*)rc->r->b)-1);
  p->n = NULL;
  rc->r->a = rc->a;
  rc->r->b = rc->b;
  free_lobjs(rc->r->lobjs);
  rc->r->lobjs = NULL;
  return 0;
}

static Heap* newHeap(void)
{
  Heap* h;
  h = (Heap*)malloc(sizeof(Heap));
  if ( h == 0 )
    die ("newHeap: couldn't allocate room for heap");
  h->status = HSTAT_UNINITIALIZED;
  h->r0copy = NULL;
  h->r2copy = NULL;
  h->r3copy = NULL;
  h->r4copy = NULL;
  h->r5copy = NULL;
  h->r6copy = NULL;
  h->sp = NULL;
  return h;
}

Heap* getHeap(void)
{
  Heap* h;

  HEAP_POOL_MUTEX_LOCK;
  if ( heapPoolIndex )
    {
      h = heapPool[--heapPoolIndex];
      HEAP_POOL_MUTEX_UNLOCK;
    }
  else   // allocate new heap
    { 
      int hid = heapid_counter++;
      HEAP_POOL_MUTEX_UNLOCK;
      h = newHeap();
      h->heapid = hid;
    }

#ifdef THREADS
  //  Ns_Log(Notice, "starting execution in heap %d", h->heapid);
#endif

  return h;
}

void touchHeap(Heap* h)
{
  if ( h->status != HSTAT_CLEAN )
    die("touchHeap: status <> HSTAT_CLEAN");
  h->status = HSTAT_DIRTY;
}

static void freePages(RegionCopy *rc)
{
  if ( rc ) 
    {
      free_region_pages(rc->r->fp, (Klump*)(rc->r->b) - 1);
      free(rc);
    }  
}

void deleteHeap(Heap *h)
{
  freePages(h->r0copy);
  freePages(h->r2copy);
  freePages(h->r3copy);
  freePages(h->r4copy);
  freePages(h->r5copy);
  freePages(h->r6copy);
  free(h);
}

void releaseHeap(Heap *h)
{
  restoreHeap(h);

  HEAP_POOL_MUTEX_LOCK;
  if ( heapPoolIndex < MAX_HEAP_POOL_SZ ) 
    {
      heapPool[heapPoolIndex++] = h;
      HEAP_POOL_MUTEX_UNLOCK;
    }
  else
    {
      HEAP_POOL_MUTEX_UNLOCK;
      deleteHeap(h);
    } 
}

static void restoreHeap(Heap *h)
{
  if ( h->status != HSTAT_DIRTY )
    die ("restoreHeap: status <> HSTAT_DIRTY");
 
  if ( restoreRegion(h->r0copy) == -1 )
    die ("restoreHeap: failed to restore r0");

  if ( restoreRegion(h->r2copy) == -1 )
    die ("restoreHeap: failed to restore r2");

  if ( restoreRegion(h->r3copy) == -1 )
    die ("restoreHeap: failed to restore r3");

  if ( restoreRegion(h->r4copy) == -1 )
    die ("restoreHeap: failed to restore r4");

  if ( restoreRegion(h->r5copy) == -1 )
    die ("restoreHeap: failed to restore r5");

  if ( restoreRegion(h->r6copy) == -1 )
    die ("restoreHeap: failed to restore r6");

  h->status = HSTAT_CLEAN;
}

void initializeHeap(Heap *h, int *sp, int *exnPtr, unsigned long exnCnt)
{
  Ro *r0, *r2, *r3, *r4, *r5, *r6; 

  if ( h->status != HSTAT_UNINITIALIZED )
    die ("initializeHeap: status <> HSTAT_UNINITIALIZED");

  r0 = clearStatusBits(*(Ro**)(h->ds));
  r2 = r0+1;
  r3 = r0+2;
  r4 = r0+3;
  r5 = r0+4;
  r6 = r0+5;
  
  h->sp = sp;
  h->exnPtr = exnPtr;
  h->exnCnt = exnCnt;

  //  printf("r0 = %x, r2 = %x, r3=%x, h=%x, ds=%x\n", r0,r2,r3,h,h->ds);

  h->r0copy = copyRegion(r0);
  h->r2copy = copyRegion(r2);
  h->r3copy = copyRegion(r3);
  h->r4copy = copyRegion(r4);
  h->r5copy = copyRegion(r5);
  h->r6copy = copyRegion(r6);

  h->status = HSTAT_CLEAN;
}

void 
clearHeapCache()
{
  Heap *h;

  HEAP_POOL_MUTEX_LOCK;
  while ( heapPoolIndex )
    {
      h = heapPool[--heapPoolIndex];
      deleteHeap(h);      
    }
  HEAP_POOL_MUTEX_UNLOCK;
  return;
}
