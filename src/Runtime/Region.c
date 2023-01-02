/*----------------------------------------------------------------*
 *                        Regions                                 *
 *----------------------------------------------------------------*/
#include <stdio.h>
#include "Flags.h"
#include "Region.h"
#include "Math.h"
#include "Profiling.h"
#include "GC.h"
#include "CommandLine.h"
#include "Locks.h"

#ifdef PARALLEL
#include "Spawn.h"
#endif

#include "Runtime.h"

/*----------------------------------------------------*
 * Hash table to collect region page reuse statistics *
 *  region_page_addr -> int                           *
 *----------------------------------------------------*/

#if ( REGION_PAGE_STAT )

RegionPageMap*
regionPageMapInsert(RegionPageMap* regionPageMap, uintptr_t addr)
{
  int index;
  RegionPageMapHashList* newElem;

  newElem = (RegionPageMapHashList*)malloc(sizeof(RegionPageMapHashList));
  if ( newElem == NULL ) {
    die("regionPageMapInsert error");
  }

  newElem->n = 1;
  newElem->addr = addr;

  index = hashRegionPageIndex(addr);
  newElem->next = regionPageMap[index];

  regionPageMap[index] = newElem;
  return regionPageMap;    /* We want to allow for hash-table
			    * resizing in the future */
}

/* Create and allocate space for a new regionPageMapHashTable */
void
regionPageMapZero(RegionPageMap* regionPageMap)
{
  int i;
  for ( i = 0 ; i < REGION_PAGE_MAP_HASH_TABLE_SIZE ; i++ )
    {
      regionPageMap[i] = NULL;
    }
}

RegionPageMap*
regionPageMapNew(void)
{
  RegionPageMap* regionPageMap;

  regionPageMap = (RegionPageMap*)malloc(sizeof(long*) * REGION_PAGE_MAP_HASH_TABLE_SIZE);
  if ( regionPageMap == NULL ) {
    die("Unable to allocate memory for RegionPageMapHashTable");
  }

  regionPageMapZero(regionPageMap);
  return regionPageMap;
}

RegionPageMap*
regionPageMapIncr(RegionPageMap* regionPageMap, uintptr_t addr)
{
  RegionPageMapHashList* p;
  for ( p = regionPageMap[hashRegionPageIndex(addr)]; p != NULL ; p = p->next )
    {
      if ( p->addr == addr )
	{
	  p->n++;
	  return regionPageMap;
	}
    }
  return regionPageMapInsert(regionPageMap,addr);
}

uintptr_t
regionPageMapLookup(RegionPageMap* regionPageMap, uintptr_t addr)
{
  RegionPageMapHashList* p;
  for ( p = regionPageMap[hashRegionPageIndex(addr)]; p != NULL ; p = p->next )
    {
      if ( p->addr == addr )
	{
	  return p->n;
	}
    }
  return (uintptr_t) NULL;
}

void
regionPageMapClear(RegionPageMap* regionPageMap)
{
  int i;
  RegionPageMapHashList *p, *n;

  for ( i = 0 ; i < REGION_PAGE_MAP_HASH_TABLE_SIZE ; i++ )
    {
      p = regionPageMap[i];
      while ( p )
	{
	  n = p->next;
	  free(p);
	  p = n;
	}
      regionPageMap[i] = 0;
    }
}

RegionPageMap* rpMap = NULL;
#define REGION_PAGE_MAP_INCR(rp) (regionPageMapIncr(rpMap,(uintptr_t)(rp)));
#else
#define REGION_PAGE_MAP_INCR(rp)
#endif /* REGION_PAGE_STAT */

/*----------------------------------------------------------------*
 * Global declarations                                            *
 *----------------------------------------------------------------*/
Rp * global_freelist = NULL;

#ifdef ENABLE_GC
long rp_used = 0;
#endif /* ENABLE_GC */
long rp_total = 0;

#ifdef PROFILING

extern long* stackBot;

FiniteRegionDesc * topFiniteRegion = NULL;

unsigned long callsOfDeallocateRegionInf=0,
             callsOfDeallocateRegionFin=0,
             callsOfAlloc=0,
             callsOfResetRegion=0,
             callsOfDeallocateRegionsUntil=0,
             callsOfAllocateRegionInf=0,
             callsOfAllocateRegionFin=0,
             callsOfSbrk=0,
             maxNoOfPages=0,
             noOfPages=0,
             allocNowInf=0,             /* Allocated in inf. regions now. */
             maxAllocInf=0,             /* Max. allocatated data in inf. regions. */
             allocNowFin=0,             /* Allocated in fin. regions now. */
             maxAllocFin=0,             /* Max. allocated in fin. regions. */
             allocProfNowInf=0,         /* Words used on object descriptors in inf. regions. */
             maxAllocProfInf=0,         /* At time maxAllocInf how much were
                                           used on object descriptors. */
             allocProfNowFin=0,         /* Words used on object descriptors in fin. regions. */
             maxAllocProfFin=0,         /* At time maxAllocFin how much were used on object descriptors. */
             maxAlloc=0,                /* Max. allocated data in both inf. and fin. regions. */
                                        /* Are not nessesarily equal to maxAllocInf+maxAllocFin!!! */
             maxMem=0,                  /* Max. allocated data on stack and in regions (finite and infinite) */
             regionDescUseInf=0,        /* Words used on non profiling information in inf. region descriptors. */
	     maxRegionDescUseInf=0,     /* Max. words used on non profiling information in inf. region descriptors. */
             regionDescUseProfInf=0,    /* Words used on profiling information in inf. region descriptors. */
	     maxRegionDescUseProfInf=0, /* Max. words used on profiling information in inf. region descriptors. */

             regionDescUseProfFin=0,    /* Words used on profiling information in fin. region descriptors. */
	     maxRegionDescUseProfFin=0, /* At time maxAllocFin, how much were used on finite region descriptors. */

             maxProfStack=0,            /* At time of max. stack size, how much is due to profiling.        */
                                        /* It is updated in function Profiling.updateMaxProfStack, which is */
                                        /* called from the assembler file.                                  */
             allocatedLobjs=0;          /* Total number of allocated large objects allocated with malloc */

inline static unsigned int
max(unsigned int a, unsigned int b)
{
  return (a<b)?b:a;
}

#endif /*PROFILING*/


/*------------------------------------------------------*
 * If an error occurs, then print the error and stop.   *
 *------------------------------------------------------*/
/*
char errorStr[255];
void printERROR(char *errorStr) {
  printf("\n***********************ERROR*****************************\n");
  printf("%s\n", errorStr);
  printf("\n***********************ERROR*****************************\n");
  exit(-1);
}
*/

/* Print info about a region. */
/*
void printTopRegInfo() {
  Ro *r;
  Rp *kp;

  r = (Ro *) clearStatusBits((int) TOP_REGION);
  printf("printRegInfo\n");
  printf("Region at address: %0x\n", r);
  printf("  fp: %0x\n", (r->g0.fp));
  printf("  a : %0x\n", (r->g0.a));
  printf("  p : %0x\n", (r->p));

  printf("Region Pages\n");
  for (kp=r->g0.fp;kp!=NULL;kp=kp->n)
    printf(" %0x\n ", kp);

  return;
}
*/

/* Print info about a region. */
void
pp_gen(Gen *gen)
{
  Rp* rp;

  fprintf(stderr,"\n[Gen g%d at addr: %p, fp:%p, a:%p, b:%p\n",
	  (is_gen_1(*gen)?1:0),
	  gen,
	  gen->fp,
	  gen->a,
	  rpBoundary(gen->a));
  for (rp = clear_fp(gen->fp) ; rp ; rp = clear_tospace_bit(rp->n)) {
#ifdef ENABLE_GEN_GC
    fprintf(stderr,"  Rp %p, next:%p, colorPtr:%p, data: %p, rp+1: %p\n",
	    rp,
	    rp->n,
	    rp->colorPtr,
	    &(rp->i),
	    rp+1);
#else
    fprintf(stderr,"  Rp %p, next:%p, data: %p, rp+1: %p\n",
	    rp,
	    rp->n,
	    &(rp->i),
	    rp+1);
#endif /* ENABLE_GEN_GC */
  }
  fprintf(stderr,"]\n");
}

void
pp_reg(Region r,  char *str)
{
  r = clearStatusBits(r);
  fprintf(stderr,"printRegionInfo called from: %s\n",str);
  fprintf(stderr,"Region at address: %p\n", r);
  pp_gen(&(r->g0));
#ifdef ENABLE_GEN_GC
  pp_gen(&(r->g1));
#endif /* ENABLE_GEN_GC */

  return;
}

void
chk_obj_in_gen(Gen *gen, uintptr_t *obj_ptr, char* s)
{
  Rp* rp;
  int found = 0;
  return;  // ToDo: GenGC remove
  for (rp = clear_fp(gen->fp) ; rp ; rp = clear_tospace_bit(rp->n)) {
    if (obj_ptr < (uintptr_t*)(rp+1) && obj_ptr >= (uintptr_t*) &(rp->i))
      found = 1;
  }
  if (! found) {
    fprintf(stderr,"chk_obj_in_gen, obj_ptr: %p not in gen:\n",obj_ptr);
    pp_reg(get_ro_from_gen(*gen),"chk_obj_in_gen");
    fprintf(stderr,"STOP:%s\n",s);
    die("");
  }
  return;
}

/*
void printRegionStack() {
  Ro *r;

  for(r=TOP_REGION;r!=NULL;r=r->p)
    printRegionInfo((int)r,"printRegionStack");

  return;
}
*/

/* Calculate number of pages in a generation */
inline size_t
NoOfPagesInGen(Gen *gen)
{
  size_t i;
  Rp *rp;

  debug(printf("[NoOfPagesInGen..."));

  for ( i = 0, rp = clear_fp(gen->fp) ; rp ; rp = clear_tospace_bit(rp->n) )
    i++;

  debug(printf("]\n"));

  return i;
}

/* Calculate number of pages in an infinite region. */
size_t
NoOfPagesInRegion(Region r)
{
#ifdef ENABLE_GEN_GC
  return NoOfPagesInGen(&(r->g0)) + NoOfPagesInGen(&(r->g1));
#else
  return NoOfPagesInGen(&(r->g0));
#endif /* ENABLE_GEN_GC */
}

/*
void
printFreeList()
{
  Rp *kp;

  printf("Enter printFreeList\n");
  FREELIST_MUTEX_LOCK;
  kp = global_freelist;
  while (kp != NULL) {
    printf(" %0x ",kp);
    kp = kp->n;
  }
  FREELIST_MUTEX_UNLOCK;
  printf("Exit printFreeList\n");
  return;
}
*/


#ifdef ENABLE_GC
size_t
size_free_list()
{
  Rp *rp;
  size_t i=0;

  LOCK_LOCK(FREELISTMUTEX);

  for ( rp = global_freelist ; rp ; rp = rp-> n )
    i++;

  LOCK_UNLOCK(FREELISTMUTEX);

  return i;
}
#endif /*ENABLE_GC*/

/*-------------------------------------------------------------------------*
 *                         Region operations.                              *
 *                                                                         *
 * allocateRegion: Allocates a region and return a pointer to it.          *
 * deallocateRegion: Pops the top region of the region stack.              *
 * callSbrk: Updates the freelist with new region pages.                   *
 * alloc: Allocates n words in a region.                                   *
 * resetRegion: Resets a region by freeing all pages except one            *
 * deallocateRegionsUntil: All regions below a threshold are deallocated.  *
 *   (stack grows downwards)                                               *
 *-------------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
 *alloc_new_page:                                                       *
 *  Allocates a new page in region.                                     *
 *  The second argument is a pointer to the generation in r to use      *
 *  Important: alloc_new_page must preserve all marks in fp (Region.h)  *
 *----------------------------------------------------------------------*/

uintptr_t *
alloc_new_page(Gen *gen)
{
  Rp* np;
  debug(printf("[alloc_new_page: gen: %p", gen);)

#ifdef PROFILING
  Ro *r;
  r = get_ro_from_gen(*gen);
#endif /* PROFILING */

#ifdef PROFILING
  profTabIncrNoOfPages(r->regionId, 1);
  profTabMaybeIncrMaxNoOfPages(r->regionId);
  maxNoOfPages = max(++noOfPages, maxNoOfPages);
#endif

  #ifdef ENABLE_GC
  rp_used++;
  if ( (!disable_gc) && (!time_to_gc) )
    {
      // the treshold suggests when we can garbage collect without allocating
      // more memory.
      //      double treshold = (double)rp_total - (((double)rp_total) / heap_to_live_ratio);
      if ( rp_used > rp_gc_treshold )
	{
	  // calculate correct value for rp_used; the current value may exceed the correct
	  // value due to conservative computation in resetRegion, deallocRegion...
	  rp_used = rp_total - size_free_list();
	  if ( rp_used > rp_gc_treshold )
	    {
	      time_to_gc = 1;
	    }
	}
    }
  #endif /* ENABLE_GC */

  MAYBE_DEFINE_CONTEXT;

  if ( FREELIST ) {
    np = FREELIST;
    FREELIST = FREELIST->n;
  } else {
#ifdef PARALLEL
    LOCK_LOCK(FREELISTMUTEX);
    if ( FREELIST == NULL ) {
      // Take half REGION_PAGE_BAG_SIZE pages from freelist and move to
      // FREELIST; allocate more pages in freelist if necessary, by
      // calling callSbrk.

      // We know for sure that there are no pages in FREELIST
      if (FREELIST) {
	die("ERROR: alloc_new_page failed; expecting empty FREELIST\n");
      }
      for ( int k = REGION_PAGE_BAG_SIZE >> 1 ; k > 0 ;  k-- ) {
	if ( global_freelist == NULL ) {
	  callSbrk();
	}
	// Now we know there are pages in global_freelist
	if ( global_freelist == NULL ) {
	  die ("ERROR: alloc_new_page failed; expecting non-empty freelist\n");
	}
	Rp * fl_tmp = FREELIST;
	FREELIST = global_freelist;
	global_freelist = global_freelist->n;
	FREELIST->n = fl_tmp;
      }
    }
    np = FREELIST;
    FREELIST = FREELIST->n;
    LOCK_UNLOCK(FREELISTMUTEX);
#else
    callSbrk();
    np = FREELIST;
    FREELIST = FREELIST->n;
#endif
  }

  REGION_PAGE_MAP_INCR(np); // update frequency hashtable

#ifdef ENABLE_GEN_GC
  // update colorPtr so that all new objects are considered to be in
  // tospace ToDo: GenGC find ud af om denne altid skal k�res eller om
  // vi kan n�jes med at inds�tte den under doing_gc colorPtr
  // opdateres EFTER GC, dvs m� der under GC v�re colorPtr som er
  // udefinerede? Det tror jeg faktisk ikke. Dem i g0 anvendes til at
  // angive farve ved what gen to alloc to og i g1 anvendes de i
  // points_in_tospace.
  np->colorPtr = (uintptr_t *)(&(np->i));
#endif /* ENABLE_GEN_GC */

#ifdef ENABLE_GC
  if ( doing_gc
#ifdef ENABLE_GEN_GC
      && ( major_p || !is_gen_1(*gen) )
#endif
       )
    np->n = set_tospace_bit(NULL);     // to-space bit
  else
#endif
    np->n = NULL;
  np->gen = gen;         // Install origin-pointer to generation - used by GC

  if ( clear_fp(gen->fp) )
#ifdef ENABLE_GC
    if ( doing_gc && is_tospace_bit(last_rp_of_gen(gen)->n) ) // keep to-space bit
      last_rp_of_gen(gen)->n = set_tospace_bit(np); // updates the next field in the last
                                                     // region page, but keeps the bit
    else
#endif
    last_rp_of_gen(gen)->n = np; // Updates the next field in the last region page.
  else {
#ifdef ENABLE_GC
    uintptr_t rt;
    if ( (rt = all_marks_fp(*gen)) /* was rtype(*gen) 2003-08-06, nh */ )
      {
	gen->fp = np;           /* Update pointer to the first page. */
	set_fp(*gen,rt);
      }
    else
#endif
      gen->fp = np;                /* Update pointer to the first page. */
  }

  debug(printf("]\n");)
  return (uintptr_t *) (&(np->i));    /* Return the allocation pointer. */
}

/*----------------------------------------------------------------------*
 *allocateRegion:                                                       *
 *  Get a first regionpage for the region.                              *
 *  Put a region administrationsstructure on the stack. The address is  *
 *  in roAddr.                                                          *
 *----------------------------------------------------------------------*/

static inline Region
allocateRegion0(Context ctx, Region r, Protect protect)
{
  debug(printf("[allocateRegion0 (rAddr=%p, protect=%zu)...",r,protect));
  r = clearStatusBits(r);

  CHECK_CTX("allocateRegion0");

  r->g0.fp = NULL;
  r->p = TOP_REGION;	                   // Push this region onto the region stack
  r->lobjs = NULL;                         // The list of large objects is empty
  r->g0.a = alloc_new_page(&(r->g0));      // Allocate the first region page in g0

#ifdef PARALLEL
  if ( protect ) { r->mutex = mutex_freelist_pop(ctx); }
  else { r->mutex = NULL; }
#endif

#ifdef ENABLE_GEN_GC
  r->g1.fp = NULL;
  set_gen_1(r->g1);                        // Mark generation
  r->g1.a = alloc_new_page(&(r->g1));      // Allocate the first region page in g1
#endif /* ENABLE_GEN_GC */

  TOP_REGION = r;

  debug(printf("]\n"));
  return r;
}

Region
allocateRegion(Context ctx, Region r, Protect p)
{
  r = allocateRegion0(ctx,r,p);
  r = (Region)setInfiniteBit((uintptr_t)r);
  return r;
}

#ifdef ENABLE_GC
Region
allocatePairRegion(Context ctx, Region r, Protect p)
{
  r = allocateRegion0(ctx,r,p);
  set_pairregion(r->g0);
#ifdef ENABLE_GEN_GC
  set_pairregion(r->g1);
#endif /* ENABLE_GEN_GC */
  r = (Region)setInfiniteBit((uintptr_t)r);
  return r;
}

Region
allocateArrayRegion(Context ctx, Region r, Protect p)
{
  r = allocateRegion0(ctx,r,p);
  set_arrayregion(r->g0);
#ifdef ENABLE_GEN_GC
  set_arrayregion(r->g1);
#endif /* ENABLE_GEN_GC */
  r = (Region)setInfiniteBit((uintptr_t)r);
  return r;
}

Region
allocateRefRegion(Context ctx, Region r, Protect p)
{
  r = allocateRegion0(ctx,r,p);
  set_refregion(r->g0);
#ifdef ENABLE_GEN_GC
  set_refregion(r->g1);
#endif /* ENABLE_GEN_GC */
  r = (Region)setInfiniteBit((uintptr_t)r);
  return r;
}

Region
allocateTripleRegion(Context ctx, Region r, Protect p)
{
  r = allocateRegion0(ctx,r,p);
  set_tripleregion(r->g0);
#ifdef ENABLE_GEN_GC
  set_tripleregion(r->g1);
#endif /* ENABLE_GEN_GC */
  r = (Region)setInfiniteBit((uintptr_t)r);
  return r;
}
#endif /*ENABLE_GC*/

void free_lobjs(Lobjs* lobjs)
{
  //if ( lobjs )
  //  fprintf(stderr, "Freeing large objs: lobjs=%p\n", lobjs);
  while ( lobjs )
    {
      Lobjs* lobjsTmp;

#ifdef ENABLE_GC
      unsigned int tag;
  #ifdef PROFILING
      tag = *((&(lobjs->value)) + sizeObjectDesc);
  #else
      tag = lobjs->value;
  #endif

      lobjs_current -= size_lobj(tag);
#endif
      lobjsTmp = clear_lobj_bit(lobjs->next);
#ifdef ENABLE_GC
      free(lobjs->orig);
#else
      free(lobjs);
#endif
      lobjs = lobjsTmp;
    }
}

/*----------------------------------------------------------------------*
 *deallocateRegion:                                                     *
 *  Pops the top region of the stack, and insert the regionpages in the *
 *  free list. There have to be atleast one region on the stack.        *
 *  When profiling we also use this function.                           *
 *----------------------------------------------------------------------*/
void deallocateRegion(Context ctx) {
#ifdef PROFILING
  int i;
#endif

  debug(printf("[deallocateRegion... top region: %p\n", TOP_REGION));

  CHECK_CTX("deallocateRegion");

#ifdef PROFILING
  callsOfDeallocateRegionInf++;
  regionDescUseInf -= (sizeRo-sizeRoProf);
  regionDescUseProfInf -= sizeRoProf;
  i = NoOfPagesInRegion(TOP_REGION);
  noOfPages -= i;
  allocNowInf -= TOP_REGION->allocNow;
  allocProfNowInf -= TOP_REGION->allocProfNow;
  profTabDecrNoOfPages(TOP_REGION->regionId, i);
  profTabDecrAllocNow(TOP_REGION->regionId, TOP_REGION->allocNow, "deallocateRegion");
#endif

#ifdef PARALLEL
  if ( TOP_REGION->mutex ) {
    mutex_freelist_push(TOP_REGION->mutex, ctx);
  }
#endif

#ifdef ENABLE_GC
  rp_used -= MIN_NO_OF_PAGES_IN_REGION;
#endif /* ENABLE_GC */

  free_lobjs(TOP_REGION->lobjs);

  //  MAYBE_DEFINE_CONTEXT;

  /* Insert the region pages in the freelist; there is always
   * at least one page in a generation. */
  last_rp_of_gen(&(TOP_REGION->g0))->n = FREELIST;  // Free pages in generation 0
  FREELIST = clear_fp(TOP_REGION->g0.fp);
#ifdef ENABLE_GEN_GC
  last_rp_of_gen(&(TOP_REGION->g1))->n = FREELIST;  // Free pages in generation 1
  FREELIST = clear_fp(TOP_REGION->g1.fp);
#endif /* ENABLE_GEN_GC */
  TOP_REGION = TOP_REGION->p;

  debug(printf("]\n"));

  return;
}

inline static Lobjs *
alloc_lobjs(int n) {
  Lobjs* lobjs;
#ifdef ENABLE_GC
  char *p;
  size_t r;
  size_t sz_bytes;
  sz_bytes = sizeof(uintptr_t)*n + sizeof(Lobjs) + sizeof(Rp);  /* ensure alignment on Rp boundaries */
  p = malloc(sz_bytes);
  if ( p == NULL )
    die("alloc_lobjs: malloc returned NULL");
  if ( (r = (size_t)p % sizeof(Rp)) ) {
    lobjs = (Lobjs*)(p + sizeof(Rp) - r);
  } else {
    lobjs = (Lobjs*)p;
  }
  //fprintf(stderr, "Allocated large obj: p=%p; r=%x; lobjs=%p; last_byte=%p; sz_bytes=%d\n", p, r, lobjs, p + sz_bytes, sz_bytes);
  if ( ! is_rp_aligned((size_t)lobjs) )
    die("alloc_lobjs: large object is not properly aligned.");
  lobjs->orig = p;
#else
  lobjs = (Lobjs*)malloc(sizeof(uintptr_t)*n + sizeof(Lobjs));
  if ( lobjs == NULL )
    die("alloc_lobjs: malloc returned NULL");
#endif /* ENABLE_GC */
  return lobjs;
}

/*----------------------------------------------------------------------*
 *callSbrk:                                                             *
 *  Sbrk is called and the free list is updated.                        *
 *  The free list has to be empty.                                      *
 *----------------------------------------------------------------------*/
void callSbrk() {
  Rp *np, *old_free_list;
  char *sb;
  size_t temp;

#ifdef PROFILING
  callsOfSbrk++;
#endif

  /* We must manually insure double alignment. Some operating systems (like *
   * HP UX) does not return a double aligned address...                     */

  /* For GC we require alignments according to the size of region pages! */

  sb = malloc(BYTES_ALLOC_BY_SBRK + sizeof(Rp) + sizeof(Rp) );

  if ( sb == NULL ) {
    perror("I could not allocate more memory; either no more memory is\navailable or the memory subsystem is detectively corrupted\n");
    exit(-1);
  }

  /* alignment (martin) */
  if (( temp = (size_t)(((uintptr_t)sb) % sizeof(Rp) ))) {
    sb = sb + sizeof(Rp) - temp;
  }

  if ( ! is_rp_aligned((size_t)sb) ) {
    printf ("sb=%p\n", sb);
    printf ("sizeof(Rp)=%ld\n", sizeof(Rp));
    printf ("sizeof(uintptr_t)=%ld\n", sizeof(uintptr_t));
    printf ("temp=%ld\n", temp);
    die("SBRK region page is not properly aligned.");
  }

  old_free_list = global_freelist;
  np = (Rp *) sb;
  global_freelist = np;

  rp_total++;

  /* fragment the SBRK-chunk into region pages */
  while ((char *)(np+1) < ((char *)global_freelist)+BYTES_ALLOC_BY_SBRK) {
    np++;
    (np-1)->n = np;
    rp_total++;
  }
  np->n = old_free_list;

  //  #ifdef ENABLE_GC
  //  if (!disable_gc)
  //    time_to_gc = 1;
  //  #endif /* ENABLE_GC */

  return;
}

/*----------------------------------------------------------------------*
 *alloc:                                                                *
 *  Allocates n words in region rAddr. It will make sure, that there    *
 *  is space for the n words before doing the allocation.               *
 *  Objects whose size n <= ALLOCATABLE_WORDS_IN_REGION_PAGE are        *
 *  allocated in region pages; larger objects are allocated using       *
 *  malloc.                                                             *
 *----------------------------------------------------------------------*/
inline uintptr_t *
allocGen (
#ifdef PARALLEL
	  Region r, Gen *gen, size_t n, int protect_p
#else
	  Gen *gen, size_t n
#endif
	  ) {
  uintptr_t *t1;
  uintptr_t *t2;
  uintptr_t *t3;

#ifndef PARALLEL
  Region r;
#endif

#if defined(PROFILING) || defined(ENABLE_GC)
  uintptr_t *i;
#endif

  debug(printf("[allocGen... generation: %p, n:%zu ", gen,n));
  debug(fflush(stdout));

#ifdef PROFILING
  r = get_ro_from_gen(*gen);
  allocNowInf += n-sizeObjectDesc; /* When profiling we also allocate an object descriptor. */
  maxAlloc = max(maxAlloc, allocNowInf+allocNowFin);
  r->allocNow += n-sizeObjectDesc;
  /*  checkProfTab("profTabIncrAllocNow.entering.alloc");  */
  profTabIncrAllocNow(r->regionId, n-sizeObjectDesc);

  callsOfAlloc++;
  maxAllocInf = max(allocNowInf, maxAllocInf);
  allocProfNowInf += sizeObjectDesc;
  if (maxAllocInf == allocNowInf) maxAllocProfInf = allocProfNowInf;
  r->allocProfNow += sizeObjectDesc;

  //  register long *stackTop asm ("rsp");
  long stackTop;
  __asm__ volatile ("movq %%rsp, %0"
		    : "=r" (stackTop)
		    );

  maxMem = max(maxMem, ((long)stackBot) - ((long)stackTop) + 8*(allocNowInf-regionDescUseProfInf-regionDescUseProfFin-allocProfNowFin));
#endif /* PROFILING */

  // see if the size of requested memory exceeds
  // the size of a region page

  if ( n > ALLOCATABLE_WORDS_IN_REGION_PAGE )   // notice: n is in words
    {
#ifdef PARALLEL
      if (protect_p && r->mutex) {
	REGION_MUTEX_LOCK((r->mutex)->mutex);
      }
#else
      r = get_ro_from_gen(*gen);
#endif
      Lobjs* lobjs;
      //#ifdef ENABLE_GC
      // fprintf(stderr,"Allocating large object (of type %lu) of %zu words\n", rtype(*gen), n);
      //#endif
      lobjs = alloc_lobjs(n);
      //fprintf(stderr,"Allocated large object of %d words (address: %p) ; header at %p\n", n, &(lobjs->value), lobjs);
      lobjs->next = set_lobj_bit(r->lobjs);
      r->lobjs = lobjs;
    #ifdef PROFILING
      allocatedLobjs++;
    #endif
#ifdef ENABLE_GC
      lobjs_current += sizeof(void*)*n;
      lobjs_period += sizeof(void*)*n;
      if ( (!disable_gc) && (lobjs_current>lobjs_gc_treshold) )
	{
	  time_to_gc = 1;
	}
#endif

#ifdef PARALLEL
      if (protect_p && r->mutex) {
	REGION_MUTEX_UNLOCK((r->mutex)->mutex);
      }
#endif

      // set the constant bit so that GC won't run
      // through the thing before there is data in the
      // object. This shouldn't be necessary; mael 2005-11-09
//    lobjs->value = set_tag_const(lobjs->value);
      return &(lobjs->value);
    }

#ifdef ENABLE_GC
  alloc_period += (sizeof(void*) * n);
#endif

#ifdef PARALLEL

  if (protect_p && r->mutex) {

    if ( 0 ) {  // simple mutex-based locking (assumes simple_p is true in CodeGenUtilX64.sml)
      REGION_MUTEX_LOCK((r->mutex)->mutex);
      t1 = gen->a;
      t2 = t1 + n;
      t3 = rpBoundary(t1);
      if (t2 > t3) {
#if defined(PROFILING) || defined(ENABLE_GC)
	for ( i = t1 ; i < t3 ; i++ )  *i = notPP;
#endif
	t1 = alloc_new_page(gen);
	t2 = t1 + n;
      }
      gen->a = t2;
      REGION_MUTEX_UNLOCK((r->mutex)->mutex);
    } else {

      // Partial lock-free atomic allocation
    start:
      t1 = gen->a;
      t2 = t1 + n;
      t3 = rpBoundary(t1);
      if (t2 <= t3) {
	if (! __atomic_compare_exchange(&(gen->a), &t1, &t2, FALSE,
					__ATOMIC_SEQ_CST,
					__ATOMIC_SEQ_CST)) {
	  goto start;  // goto start if gen->a <> t1
	}
      } else {
	// t2 > t3  -- not enough space for object
	REGION_MUTEX_LOCK((r->mutex)->mutex);
	//printf("taking lock on %p\n", gen);
      locked_start:
	t1 = gen->a;
	t2 = t1 + n;
	t3 = rpBoundary(t1);
	if (t2 <= t3) {
	  // now there is space; some other thread has allocated a page - try again
	  if (! __atomic_compare_exchange(&(gen->a), &t1, &t2, FALSE,
					  __ATOMIC_SEQ_CST,
					  __ATOMIC_SEQ_CST)) {
	    // Someone without the lock has updated the allocation pointer, so we try again (we have the lock)
	    goto locked_start;  // goto start if gen->a <> t1
	  }
	  // Allocation succeeded! Unlock and proceed to exit
	  REGION_MUTEX_UNLOCK((r->mutex)->mutex);
	  goto finish; // t1 contains the pointer to the allocated memory and gen->a has been updated
	}
	// t2 > t3
	// Attempt to store the boundary in gen->a
	if (! __atomic_compare_exchange(&(gen->a), &t1, &t3, FALSE,
					__ATOMIC_SEQ_CST,
					__ATOMIC_SEQ_CST)) {
	  // Somebody else who hasn't got the lock has managed to update the allocation pointer - we try again...
	  goto locked_start;
	}
	// After the success of the above atomic op, assembler allocation code executed by another thread cannot update the allocation pointer
	// We
#if defined(PROFILING) || defined(ENABLE_GC)
	/* insert zeros in the rest of the current region page;
	 * mael 2019-01-28: why is this necessary when just GC is enabled?
	 * WELL, we need it for printing statistics with -verbose_gc... */
	for ( i = t1 ; i < t3 ; i++ )  *i = notPP;
#endif
	t1 = alloc_new_page(gen);
	t2 = t1+n;
	asm volatile("mfence":::"memory"); // Prevent CPU & compiler reordering
	gen->a = t2;
	REGION_MUTEX_UNLOCK((r->mutex)->mutex);
      }
    }
  } else { // no protection
#endif
    t1 = gen->a;
    t2 = t1 + n;
    t3 = rpBoundary(t1);
    if (t2 > t3) {
#if defined(PROFILING) || defined(ENABLE_GC)
      /* insert zeros in the rest of the current region page;
       * mael 2019-01-28: why is this necessary when just GC is enabled?
       * WELL, we need it for printing statistics with -verbose_gc... */
      for ( i = t1 ; i < t3 ; i++ )  *i = notPP;
#endif
      gen->a = alloc_new_page(gen);
      t1 = gen->a;
      t2 = t1+n;
    }
    gen->a = t2;

#ifdef PARALLEL
  } // no protection

 finish:
#endif

  #ifdef ENABLE_GC
  #ifdef CHECK_GC
  if ( points_into_dataspace(t1) ) {
    die("allocated value points into dataspace");
  }
  #endif /* CHECK_GC */
  #endif /* ENABLE_GC */

  debug(printf(", t1=%p, t2=%p]\n", t1,t2));
  debug(fflush(stdout));

  return t1;
}

uintptr_t *alloc (Region r, size_t n) {
  r = clearStatusBits(r);
  return allocGen(
#ifdef PARALLEL
		  r, &(r->g0), n, TRUE
#else
		  &(r->g0), n
#endif
		  );
}

#ifdef PARALLEL
uintptr_t *alloc_unprotected (Region r, size_t n) {
  r = clearStatusBits(r);
  return allocGen(r, &(r->g0), n, FALSE);
}
#endif

/*----------------------------------------------------------------------*
 *resetRegion:                                                          *
 *  All regionpages except one are inserted into the free list, and     *
 *  the region administration structure is updated. The statusbits are  *
 *  not changed.                                                        *
 *----------------------------------------------------------------------*/
static inline
void resetGen(Gen *gen)
{
  /* There is always at least one page in a generation. */
  if ( (clear_fp(gen->fp))->n ) { /* There are more than one page in the generation. */

#ifdef ENABLE_GC
    rp_used--;              // at least one page is freed; see comment in alloc_new_page
                            //   concerning conservative computation.
#endif /* ENABLE_GC */

    MAYBE_DEFINE_CONTEXT;

    (last_rp_of_gen(gen))->n = FREELIST;
    FREELIST = (clear_fp(gen->fp))->n;
    (clear_fp(gen->fp))->n = NULL;
  }

  gen->a = (uintptr_t *)(&((clear_fp(gen->fp))->i));   /* beginning of data in first page */
#ifdef ENABLE_GEN_GC
  (clear_fp(gen->fp))->colorPtr = gen->a;      /* beginning of data in first page */
#endif /* ENABLE_GEN_GC */

  return;
}

Region
resetRegion(Region rAdr)
{
  Ro *r;

#ifdef PROFILING
  int j;
#endif

  debug(printf("[resetRegions..."));

  r = clearStatusBits(rAdr);

#ifdef PROFILING
  callsOfResetRegion++;
  j = NoOfPagesInRegion(r);

  /* There is always at-least one page in a generation. */
  noOfPages -= j-MIN_NO_OF_PAGES_IN_REGION;
  profTabDecrNoOfPages(r->regionId, j-MIN_NO_OF_PAGES_IN_REGION);

  allocNowInf -= r->allocNow;
  profTabDecrAllocNow(r->regionId, r->allocNow, "resetRegion");
  allocProfNowInf -= r->allocProfNow;
#endif

  resetGen(&(r->g0));
#ifdef ENABLE_GEN_GC
  resetGen(&(r->g1));
#endif /* ENABLE_GEN_GC */

  free_lobjs(r->lobjs);

  r->lobjs = NULL;

#ifdef PROFILING
  r->allocNow = 0;
  r->allocProfNow = 0;
#endif

  debug(printf("]\n"));

  return rAdr; /* We preserve rAdr and the status bits. */
}

/*-------------------------------------------------------------------------*
 * deallocateRegionsUntil:                                                 *
 *  It is called with rAddr=sp, which do not necessarily point at a region *
 *  description. It deallocates all regions that are placed under sp.      *
 * (notice: the stack is growing downwards                                 *
 *-------------------------------------------------------------------------*/
void
deallocateRegionsUntil(Context ctx, Region r)
{
  //  debug(printf("[deallocateRegionsUntil(r = %x, topFiniteRegion = %x)...\n", r, topFiniteRegion));

  debug(printf("[deallocateRegionsUntil(r = %p, topr= %p)...\n", r, TOP_REGION));

  r = clearStatusBits(r);

#ifdef PROFILING
  callsOfDeallocateRegionsUntil++;

  /* Don't call deallocRegionFiniteProfiling if no finite
   * regions are allocated. mael 2001-03-20 */
  while ( topFiniteRegion && (FiniteRegionDesc *)r >= topFiniteRegion)
    {
      deallocRegionFiniteProfiling();
    }
#endif

  while (TOP_REGION && r >= TOP_REGION)
    {
      debug(printf("r: %p, top region %p\n",r,TOP_REGION));
      deallocateRegion(ctx);
    }

  debug(printf("]\n"));

  return;
}

/*----------------------------------------------------------------*
 *        Profiling functions                                     *
 *----------------------------------------------------------------*/
#ifdef PROFILING

/***************************************************************************
 *     Changed runtime operations for making profiling possible.           *
 *                                                                         *
 * allocRegionInfiniteProfiling(roAddr, regionId)                          *
 * allocRegionFiniteProfiling(rdAddr, regionId, size)                      *
 * deallocRegionFiniteProfiling(void)                                      *
 * allocProfiling(rAddr, n, pPoint)                                        *
 ***************************************************************************/

/*----------------------------------------------------------------------*
 *allocRegionInfiniteProfiling:                                         *
 *  Get a first regionpage for the region.                              *
 *  Put a region administration structure on the stack. The address is  *
 *  in roAddr. The name of the region is regionId                       *
 *  There has to be room for the region descriptor on the stack, which  *
 *  roAddr points at.                                                   *
 *----------------------------------------------------------------------*/
Region
allocRegionInfiniteProfiling(Context ctx, Region r, size_t regionId)
{
  /* printf("[allocRegionInfiniteProfiling r=%x, regionId=%d...", r, regionId);*/

  callsOfAllocateRegionInf++;
  regionDescUseInf += (sizeRo-sizeRoProf);
  maxRegionDescUseInf = max(maxRegionDescUseInf,regionDescUseInf);
  regionDescUseProfInf += sizeRoProf;
  maxRegionDescUseProfInf = max(maxRegionDescUseProfInf,regionDescUseProfInf);

  r->p = TOP_REGION;	         // Push this region onto the region stack
  r->allocNow = 0;               // No allocation yet
  r->allocProfNow = 0;           // No allocation yet
  r->regionId = regionId;        // Put name of region in region descriptor

  r->lobjs = NULL;               // The list of large objects is empty

  r->g0.fp = NULL;
  (&(r->g0))->a = alloc_new_page(&(r->g0));     // Allocate the first region page in g0

#ifdef ENABLE_GEN_GC
  r->g1.fp = NULL;
  set_gen_1(r->g1);              // Mark generation
  (&(r->g1))->a = alloc_new_page(&(r->g1));     // Allocate the first region page in g1

#endif /* ENABLE_GEN_GC */

  TOP_REGION = r;

  r = (Region)setInfiniteBit((uintptr_t)r);

  debug(printf("exiting]\n"));

  return r;
}

/* In CodeGenX64, we use a generic function to compile a C-call. The regionId */
/* may therefore be tagged, which this stub-function takes care of.           */
Region
allocRegionInfiniteProfilingMaybeUnTag(Context ctx, Region r, size_t regionId)
{
  return allocRegionInfiniteProfiling(ctx, r, convertIntToC(regionId));
}

#ifdef ENABLE_GC
Region
allocPairRegionInfiniteProfiling(Context ctx, Region r, size_t regionId)
{
  r = allocRegionInfiniteProfiling(ctx, r, regionId);
  set_pairregion(clearStatusBits(r)->g0);
#ifdef ENABLE_GEN_GC
  set_pairregion(clearStatusBits(r)->g1);
#endif /* ENABLE_GEN_GC */
  return r;
}

Region
allocArrayRegionInfiniteProfiling(Context ctx, Region r, size_t regionId)
{
  r = allocRegionInfiniteProfiling(ctx, r, regionId);
  set_arrayregion(clearStatusBits(r)->g0);
#ifdef ENABLE_GEN_GC
  set_arrayregion(clearStatusBits(r)->g1);
#endif /* ENABLE_GEN_GC */

  return r;
}

Region
allocRefRegionInfiniteProfiling(Context ctx, Region r, size_t regionId)
{
  r = allocRegionInfiniteProfiling(ctx, r, regionId);
  set_refregion(clearStatusBits(r)->g0);
#ifdef ENABLE_GEN_GC
  set_refregion(clearStatusBits(r)->g1);
#endif /* ENABLE_GEN_GC */

  return r;
}

Region
allocTripleRegionInfiniteProfiling(Context ctx, Region r, size_t regionId)
{
  r = allocRegionInfiniteProfiling(ctx, r, regionId);
  set_tripleregion(clearStatusBits(r)->g0);
#ifdef ENABLE_GEN_GC
  set_tripleregion(clearStatusBits(r)->g1);
#endif /* ENABLE_GEN_GC */

  return r;
}

Region
allocPairRegionInfiniteProfilingMaybeUnTag(Context ctx, Region r, size_t regionId)
{
  r = allocRegionInfiniteProfiling(ctx, r, convertIntToC(regionId));
  set_pairregion(clearStatusBits(r)->g0);
#ifdef ENABLE_GEN_GC
  set_pairregion(clearStatusBits(r)->g1);
#endif /* ENABLE_GEN_GC */

  return r;
}

Region
allocArrayRegionInfiniteProfilingMaybeUnTag(Context ctx, Region r, size_t regionId)
{
  r = allocRegionInfiniteProfiling(ctx, r, convertIntToC(regionId));
  set_arrayregion(clearStatusBits(r)->g0);
#ifdef ENABLE_GEN_GC
  set_arrayregion(clearStatusBits(r)->g1);
#endif /* ENABLE_GEN_GC */

  return r;
}

Region
allocRefRegionInfiniteProfilingMaybeUnTag(Context ctx, Region r, size_t regionId)
{
  r = allocRegionInfiniteProfiling(ctx, r, convertIntToC(regionId));
  set_refregion(clearStatusBits(r)->g0);
#ifdef ENABLE_GEN_GC
  set_refregion(clearStatusBits(r)->g1);
#endif /* ENABLE_GEN_GC */

  return r;
}

Region
allocTripleRegionInfiniteProfilingMaybeUnTag(Context ctx, Region r, size_t regionId)
{
  r = allocRegionInfiniteProfiling(ctx, r, convertIntToC(regionId));
  set_tripleregion(clearStatusBits(r)->g0);
#ifdef ENABLE_GEN_GC
  set_tripleregion(clearStatusBits(r)->g1);
#endif /* ENABLE_GEN_GC */

  return r;
}
#endif /*ENABLE_GC*/

/*-------------------------------------------------------------------------------*
 * allocRegionFiniteProfiling:                                                   *
 * Program point 0 is used as indication no object at all in the runtime system. *
 * Program point 1 is used when a finite region is allocated but the correct     *
 * program point is not known.                                                   *
 * The first correct program point is 2.                                         *
 * There has to be room on the stack for the finite region descriptor and the    *
 * object descriptor. rdAddr points at the region descriptor when called.        *
 *-------------------------------------------------------------------------------*/
#define notPrgPoint 1
void
allocRegionFiniteProfiling(FiniteRegionDesc *rdAddr, size_t regionId, size_t size)
{
  ObjectDesc *objPtr;
/*
  printf("[Entering allocRegionFiniteProfiling, rdAddr=%x, regionId=%d, size=%d ...\n", rdAddr, regionId, size);
*/
  allocNowFin += size;                                  /* necessary for graph drawing */
  maxAlloc = max(maxAlloc, allocNowFin+allocNowInf);    /* necessary for graph drawing */

  callsOfAllocateRegionFin++;
  maxAllocFin = max(allocNowFin, maxAllocFin);
  allocProfNowFin += sizeObjectDesc;
  regionDescUseProfFin += sizeFiniteRegionDesc;
  if (allocNowFin == maxAllocFin) {
    maxAllocProfFin = allocProfNowFin;
    maxRegionDescUseProfFin = regionDescUseProfFin;
  }
  /*  checkProfTab("profTabIncrAllocNow.entering.allocRegionFiniteProfiling"); */
  profTabIncrAllocNow(regionId, size);

  rdAddr->p = topFiniteRegion;   /* link to previous region description on stack */
  rdAddr->regionId = regionId;   /* put name on region in descriptor. */
  topFiniteRegion = rdAddr;      /* pointer to topmost region description on stack */

  objPtr = (ObjectDesc *)(rdAddr + 1); /* We also put the object descriptor onto the stack. */
  objPtr->atId = notPrgPoint;
  objPtr->size = size;

  debug(printf("exiting, topFiniteRegion = %p, topFiniteRegion->p = %p, &topFiniteRegion = %p]\n",
  	       topFiniteRegion, topFiniteRegion->p, &topFiniteRegion));

  return;
}

/* In CodeGenX64, we use a generic function to compile a C-call. The regionId */
/* and size may therefore be tagged, which this stub-function takes care of.  */
void
allocRegionFiniteProfilingMaybeUnTag(FiniteRegionDesc *rdAddr, size_t regionId, size_t size)
{
  allocRegionFiniteProfiling(rdAddr, convertIntToC(regionId), convertIntToC(size));
  return;
}

/*-----------------------------------------------------------------*
 * deallocRegionFiniteProfiling:                                   *
 * topFiniteRegion has to point at the bottom address of the       *
 * finite region descriptor, which will be the new stack address.  *
 *-----------------------------------------------------------------*/
void
deallocRegionFiniteProfiling(void)
{
  long size;

  /*
  printf("[Entering deallocRegionFiniteProfiling regionId=%d (topFiniteRegion = %x)...\n",
	 topFiniteRegion->regionId, topFiniteRegion);
  */
  size = ((ObjectDesc *) (topFiniteRegion + 1))->size;
  allocNowFin -= size;                                    /* necessary for graph drawing */

  callsOfDeallocateRegionFin++;
  profTabDecrAllocNow(topFiniteRegion->regionId, size, "deallocRegionFiniteProfiling");
  allocProfNowFin -= sizeObjectDesc;
  regionDescUseProfFin -= sizeFiniteRegionDesc;

  topFiniteRegion = topFiniteRegion->p;                   /* pop ptr. to prev. region desc. */

  debug(printf("exiting, topFiniteRegion = %p]\n", topFiniteRegion));
}


/*-----------------------------------------------------------------*
 * allocProfiling:                                                 *
 * Same as alloc, except that an object descriptor is created.     *
 * In particular, n is still the number of words requested for     *
 * user values (not including the object descriptor).  However,    *
 * allocProfiling asks alloc for space for the object descriptor   *
 * and takes care of allocating it, returning a pointer to the     *
 * beginning of the user value, as if profiling is not enabled.    *
 *-----------------------------------------------------------------*/
uintptr_t *
allocGenProfiling(Gen *gen, size_t n, size_t pPoint)
{
  uintptr_t *res;

  debug(printf("[Entering allocProfiling... gen:%p, n:%zu, pp:%zu.", gen, n, pPoint));

  res = allocGen(gen, n+sizeObjectDesc);       // allocate object descriptor and object

  ((ObjectDesc *)res)->atId = pPoint;     // initialize object descriptor
  ((ObjectDesc *)res)->size = n;

  res = (uintptr_t *)(((ObjectDesc *)res) + 1); // return pointer to user data

  debug(printf("exiting]\n"));
  return res;
}

uintptr_t *
allocProfiling(Region r, size_t n, size_t pPoint)
{
  r = clearStatusBits(r);
  return allocGenProfiling(&(r->g0),n,pPoint);
}
#endif /*PROFILING*/
