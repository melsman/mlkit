/*----------------------------------------------------------------*
 *                        Regions                                 *
 *----------------------------------------------------------------*/
#include "Flags.h"
#include "Region.h"
#include "Math.h"
#include "Profiling.h"
#include "GC.h"
#include "CommandLine.h"

#ifdef THREADS
#include "/usr/share/aolserver/include/ns.h"
extern Ns_Mutex freelistMutex;
#define FREELIST_MUTEX_LOCK     Ns_LockMutex(&freelistMutex);
#define FREELIST_MUTEX_UNLOCK   Ns_UnlockMutex(&freelistMutex);
#else
#define FREELIST_MUTEX_LOCK
#define FREELIST_MUTEX_UNLOCK
#endif

/*----------------------------------------------------------------*
 * Global declarations                                            *
 *----------------------------------------------------------------*/
Klump * freelist = NULL;

#ifndef KAM
Ro * topRegion;
#endif

#ifdef ENABLE_GC
int rp_to_space = 0;
int rp_used = 0;
#endif /* ENABLE_GC */
int rp_total = 0;

#ifdef PROFILING
FiniteRegionDesc * topFiniteRegion = NULL;

unsigned int callsOfDeallocateRegionInf=0,
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
#endif /*PROFILING*/


static unsigned int max(unsigned int a, unsigned int b) {
  return (a<b)?b:a;
}

/*------------------------------------------------------*
 * If an error occurs, then print the error and stop.   *
 *------------------------------------------------------*/
char errorStr[255];
void printERROR(char *errorStr) {
  printf("\n***********************ERROR*****************************\n");
  printf("%s\n", errorStr);
  printf("\n***********************ERROR*****************************\n");
  exit(-1);
}

/* Print info about a region. */
/*
void printTopRegInfo() {
  Ro *rp;
  Klump *kp;

  rp = (Ro *) clearStatusBits((int) TOP_REGION);
  printf("printRegInfo\n");
  printf("Region at address: %0x\n", rp);
  printf("  fp: %0x\n", (rp->fp));
  printf("  b : %0x\n", (rp->b));
  printf("  a : %0x\n", (rp->a));
  printf("  p : %0x\n", (rp->p));

  printf("Region Pages\n");
  for (kp=rp->fp;kp!=NULL;kp=kp->k.n)
    printf(" %0x\n ", kp);

  return;
}
*/

/* Print info about a region. */
/*
void printRegionInfo(int rAddr,  char *str) {
  Ro *rp;
  Klump *kp;

  rp = (Ro *) clearStatusBits(rAddr);
  printf("printRegionInfo called from: %s\n",str);
  printf("Region at address: %0x\n", rp);
  printf("  fp: %0x\n", (rp->fp));
  printf("  b : %0x\n", (rp->b));
  printf("  a : %0x\n", (rp->a));
  printf("  p : %0x\n", (rp->p));

  printf("Region Pages\n");
  for (kp=rp->fp;kp!=NULL;kp=kp->k.n)
    printf(" %0x\n ", kp);

  return;
}

void printRegionStack() {
  Ro *rp;

  for(rp=TOP_REGION;rp!=NULL;rp=rp->p)
    printRegionInfo((int)rp,"printRegionStack");

  return;
}
*/

/* Calculate number of pages in an infinite region. */
int NoOfPagesInRegion(Ro *rp) {
  int i;
  Klump *ptr;
  for (i=0,ptr=rp->fp;ptr!=NULL;ptr=ptr->k.n) i++;
  return i;
}

/*
void printFreeList() {
  Klump *kp;

  printf("Enter printFreeList\n");
  FREELIST_MUTEX_LOCK;
  kp = freelist;
  while (kp != NULL) {
    printf(" %0x ",kp);
    kp = kp->k.n;
  }
  FREELIST_MUTEX_UNLOCK;
  printf("Exit printFreeList\n");
  return;
}
*/

#ifdef ENABLE_GC
int size_free_list() {
  Klump *kp;
  int i=0;

  FREELIST_MUTEX_LOCK;

  kp = freelist;
  while (kp != NULL) {
    i++;
    kp = kp->k.n;
  }

  FREELIST_MUTEX_UNLOCK;

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
 * deallocateRegionsUntil: All regions above a threshold are deallocated.  *
 * deallocateRegionsUntil_X86: ---- for stack growing towards -inf         * 
 *-------------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
 *allocateRegion:                                                       *
 *  Get a first regionpage for the region.                              *
 *  Put a region administrationsstructure on the stack. The address is  *
 *  in roAddr.                                                          *
 *----------------------------------------------------------------------*/
int *allocateRegion(Ro *roAddr
#ifdef KAM
		    , Ro** topRegionCell
#endif
		    ) { 
  Klump *kp;
  
  debug(printf("[allocateRegion..."));  

  roAddr = (Ro *) clearStatusBits((int)roAddr);

  FREELIST_MUTEX_LOCK;

  if (freelist==NULL) callSbrk();

  #ifdef ENABLE_GC
  rp_used++;
  #endif /* ENABLE_GC */

  kp = freelist;
  freelist= freelist->k.n;

  FREELIST_MUTEX_UNLOCK;

  kp->k.n = NULL;                  // First and last region page
  kp->k.r = roAddr;                // Point back To region descriptor

  roAddr->a = (int *)(&(kp->k.i)); // We allocate from k.i in the page
  roAddr->b = (int *)(kp+1);       // The border is after this page
  roAddr->p = TOP_REGION;	   // Push this region onto the region stack
  roAddr->fp = kp;                 // Update pointer to the first page
  roAddr->lobjs = NULL;            // The list of large objects is empty
  
  TOP_REGION = roAddr;

  // We have to set the infinitebit
  roAddr = (Ro *) setInfiniteBit((int) roAddr);

  debug(printf("]\n"));

  return (int *) roAddr;
}  

/*----------------------------------------------------------------------*
 * NEW NEW NEW NEW NEW                                                  *
 * We may not change sp!                                                *
 *deallocateRegionNew:                                                  *
 *  Pops the top region of the stack, and insert the regionpages in the *
 *  free list. There have to be atleast one region on the stack.        *
 *  When profiling we also use this function.                           *
 *----------------------------------------------------------------------*/
void deallocateRegionNew(
#ifdef KAM
			 Ro** topRegionCell
#endif
			 ) { 
  int i;

  /* printf("[deallocateRegionNew... top region: %x, regionId: %d\n", TOP_REGION, TOP_REGION->regionId);*/

#ifdef PROFILING
  callsOfDeallocateRegionInf++;
  regionDescUseInf -= (sizeRo-sizeRoProf);
  regionDescUseProfInf -= sizeRoProf;
  i = NoOfPagesInRegion(TOP_REGION);
  noOfPages -= i;
  allocNowInf -= TOP_REGION->allocNow;
  allocProfNowInf -= TOP_REGION->allocProfNow;
  profTabDecrNoOfPages(TOP_REGION->regionId, i);
  profTabDecrAllocNow(TOP_REGION->regionId, TOP_REGION->allocNow, "deallocateRegionNew");
#endif

  #ifdef ENABLE_GC
  rp_used--;
  #endif /* ENABLE_GC */

  // free large objects
  if ( TOP_REGION->lobjs )
    {
      Lobjs* lobjs = TOP_REGION->lobjs;
      while ( lobjs ) 
	{
	  Lobjs* lobjsTmp = lobjs->next;
#ifdef ENABLE_GC
	  lobj_current -= size_lobj(lobjs->value);
#endif	  
	  free(lobjs);
	  lobjs = lobjsTmp;
	}
    }

  /* Insert the region pages in the freelist; there is always 
   * at-least one page in a region. */

  FREELIST_MUTEX_LOCK;
  (((Klump *)TOP_REGION->b)-1)->k.n = freelist;
  freelist = TOP_REGION->fp;
  FREELIST_MUTEX_UNLOCK;

  TOP_REGION=TOP_REGION->p;

  /* printf("]\n");*/

  return;
}

/*----------------------------------------------------------------------*
 *callSbrk:                                                             *
 *  Sbrk is called and the free list is updated.                        *
 *  The free list has to be empty.                                      *
 *----------------------------------------------------------------------*/
void callSbrk() { 
  Klump *np, *old_free_list;
  char *sb;
  int temp;

#ifdef PROFILING
  callsOfSbrk++;
#endif

  /* We must manually insure double alignment. Some operating systems (like *
   * HP UX) does not return a double aligned address...                     */

  /* For GC we require 1Kb alignments, that is the size of a region page! */

  sb = (char *)malloc(BYTES_ALLOC_BY_SBRK + 1024 /*8*/);

  if (sb == (char *)NULL) {
    perror("I could not allocate more memory; either no more memory is\navailable or the memory subsystem is detectively corrupted\n");
    exit(-1);
  }

  /* alignment (martin) */
  if (temp=((int)sb % 1024 /*8*/)) {
    sb = (char *) (((int)sb) + 1024 /*8*/ - temp);
  }

  if (!is_rp_aligned((unsigned int)sb))
    die("SBRK region page is not properly aligned.");

  old_free_list = freelist;
  np = (Klump *) sb;
  freelist = np;

  rp_total++;

  /* We have to fragment the SBRK-chunk into region pages. */
  while ((char *)(np+1) < ((char *)freelist)+BYTES_ALLOC_BY_SBRK) { 
    np++;
    (np-1)->k.n = np;
    rp_total++;
  }
  np->k.n = old_free_list;

  #ifdef ENABLE_GC
  if (!disable_gc)
    time_to_gc = 1;
  #endif /* ENABLE_GC */

  return;
}

#ifdef ENABLE_GC
void callSbrkArg(int no_of_region_pages) { 
  Klump *np, *old_free_list;
  char *sb;
  int temp;
  int bytes_to_alloc;

#ifdef PROFILING
  callsOfSbrk++;
#endif

  /* We must manually insure double alignment. Some operating systems (like *
   * HP UX) does not return a double aligned address...                     */

  /* For GC we require 1Kb alignments, that is the size of a region page! */
  if (no_of_region_pages < REGION_PAGE_BAG_SIZE)
    no_of_region_pages = REGION_PAGE_BAG_SIZE;
  bytes_to_alloc = no_of_region_pages*sizeof(Klump);

  sb = (char *)malloc(bytes_to_alloc + 1024 /*8*/);

  if (sb == (char *)NULL) {
    perror("I could not allocate more memory; either no more memory is\navailable or the memory subsystem is detectively corrupted\n");
    exit(-1);
  }

  /* alignment (martin) */
  if (temp=((int)sb % 1024 /*8*/)) {
    sb = (char *) (((int)sb) + 1024 /*8*/ - temp);
  }

  if (!is_rp_aligned((unsigned int)sb))
    die("SBRK region page is not properly aligned.");

  /* The free list is not necessarily empty */
  old_free_list = freelist;
  np = (Klump *) sb;
  freelist = np;

  rp_total++;

  /* We have to fragment the SBRK-chunk into region pages. */
  while ((char *)(np+1) < ((char *)freelist)+bytes_to_alloc) { 
    np++;
    (np-1)->k.n = np;


    rp_total++;

  }
  np->k.n = old_free_list;

  #ifdef ENABLE_GC
  if (!disable_gc)
    time_to_gc = 1;
  #endif /* ENABLE_GC */

  return;
}
#endif /* ENABLE_GC */

/*----------------------------------------------------------------------*
 *alloc_new_block:                                                      *
 *  Allocates a new block in region.                                    *
 *----------------------------------------------------------------------*/

void alloc_new_block(Ro *rp) { 
  Klump* np;

#ifdef PROFILING
  profTabIncrNoOfPages(rp->regionId, 1);
  profTabMaybeIncrMaxNoOfPages(rp->regionId);
  maxNoOfPages = max(++noOfPages, maxNoOfPages);
#endif

  #ifdef ENABLE_GC
  rp_to_space++;
  rp_used++;
  if ( (!disable_gc) && (!time_to_gc) ) 
    {
      double treshold = (double)rp_total - (((double)rp_total) / heap_to_live_ratio);
      if ( (double)rp_used > treshold )
	{
	  // calculate correct value for rp_used; the current value may exceed the correct
	  // value due to conservative computation in resetRegion...
	  rp_used = rp_total - size_free_list();
	  if ( (double)rp_used > treshold )
	    {
	      time_to_gc = 1;
	    }
	}
    }
  #endif /* ENABLE_GC */

  FREELIST_MUTEX_LOCK;
  if (freelist==NULL) callSbrk(); 
  np = freelist;
  freelist= freelist->k.n;
  FREELIST_MUTEX_UNLOCK;

  if (!is_rp_aligned((unsigned int)np))
    die("alloc_new_block: region page is not properly aligned.");

  np->k.n = NULL;
  np->k.r = rp;      /* We Point Back To Region Descriptor. Used By GC. */

  if (rp->fp)
    (((Klump *)(rp->b))-1)->k.n = np; /* Updates the next field in the last region page. */
  else
    rp->fp = np;                    /* Update pointer to the first page. */

  rp->a = (int *)(&(np->k.i));    /* Updates the allocation pointer. */
  rp->b = (int *)(np+1);          /* Updates the border pointer. */
}

/*----------------------------------------------------------------------*
 *alloc:                                                                *
 *  Allocates n words in region rAddr. It will make sure, that there    *
 *  is space for the n words before doing the allocation.               *
 *  Objects whose size n <= ALLOCATABLE_WORDS_IN_REGION_PAGE are        *
 *  allocated in region pages; larger objects are allocated using       *
 *  malloc.
 *----------------------------------------------------------------------*/
int *alloc (int rAddr, int n) { 
  int *t1;
  int *t2;
  int *t3;
  Ro *rp;

#if defined(PROFILING) || defined(ENABLE_GC)
  int *i;
#endif

#ifdef ENABLE_GC
  alloc_period += 4*n;
#endif

  /*  debug(printf("[alloc, rAddr=%x, n=%d, topFiniteRegion = %x, ...\n", rAddr, 9, topFiniteRegion)); */
  rp = (Ro *) clearStatusBits(rAddr);

  /*  debug(printf("[alloc, rAddr=%x, id=%d, n=%d, topFiniteRegion = %x ...\n", rAddr, rp->regionId, n, topFiniteRegion)); */

  /*  printRegionStack();*/
  /*  printRegionInfo((int)rp,"from alloc ");*/

#ifdef PROFILING
  allocNowInf += n-sizeObjectDesc; /* When profiling we also allocate an object descriptor. */
  maxAlloc = max(maxAlloc, allocNowInf+allocNowFin);
  rp->allocNow += n-sizeObjectDesc;
  /*  checkProfTab("profTabIncrAllocNow.entering.alloc");  */
  profTabIncrAllocNow(rp->regionId, n-sizeObjectDesc);

  callsOfAlloc++;
  maxAllocInf = max(allocNowInf, maxAllocInf);
  allocProfNowInf += sizeObjectDesc;
  if (maxAllocInf == allocNowInf) maxAllocProfInf = allocProfNowInf;
  rp->allocProfNow += sizeObjectDesc;
#endif

  // see if the size of requested memory exceeds 
  // the size of a region page */

  if ( n > ALLOCATABLE_WORDS_IN_REGION_PAGE )   // notice: n is in words
    {
      Lobjs* lobjs;
      lobjs = (Lobjs*)malloc(4*(n+1));
      lobjs->next = rp->lobjs;
      rp->lobjs = lobjs;
      #ifdef PROFILING
      //    fprintf(stderr,"Allocating Large Object %d bytes\n", 4*n);
      allocatedLobjs++;
      #endif
#ifdef ENABLE_GC
      lobj_current += 4*n;
      if ( (!disable_gc) && (!time_to_gc) && (lobj_current>lobj_gc_treshold) ) 
	{
	  time_to_gc = 1;
	}
#endif	  
      return &(lobjs->value);
    }

  t1 = rp->a;
  t2 = t1 + n;

  t3 = rp->b;
  if (t2 > t3) {
    #if defined(PROFILING) || defined(ENABLE_GC)
       /* insert zeros in the rest of the current region page */
       for ( i = t1 ; i < t3 ; i++ )  *i = notPP;
    #endif 
    alloc_new_block(rp);

    t1 = rp->a;
    t2 = t1+n;
  }
  rp->a = t2;

  debug(printf("]\n"));

  return t1;
}

/*----------------------------------------------------------------------*
 *resetRegion:                                                          *
 *  All regionpages except one are inserted into the free list, and     *
 *  the region administration structure is updated. The statusbits are  *
 *  not changed.                                                        *
 *----------------------------------------------------------------------*/
int resetRegion(int rAdr) { 
  Ro *rp;
  
#ifdef PROFILING
  int *i;
  Klump *temp;
  int j;
#endif

  debug(printf("[resetRegions..."));

  rp = (Ro *) clearStatusBits(rAdr);

#ifdef PROFILING
  callsOfResetRegion++;
  j = NoOfPagesInRegion(rp);

  /* There is always at-least one page in a region. */
  noOfPages -= j-1;
  profTabDecrNoOfPages(rp->regionId, j-1);

  allocNowInf -= rp->allocNow;
  profTabDecrAllocNow(rp->regionId, rp->allocNow, "resetRegion");
  allocProfNowInf -= rp->allocProfNow;
#endif

  /* There is always at least one page in a region. */
  if ( (rp->fp)->k.n != NULL ) { /* There are more than one page in the region. */
    FREELIST_MUTEX_LOCK;
    (((Klump *)rp->b)-1)->k.n = freelist;
    freelist = (rp->fp)->k.n;
    FREELIST_MUTEX_UNLOCK;
    (rp->fp)->k.n = NULL;

    #ifdef ENABLE_GC
    rp_used--;              // at least one page is freed; see comment in alloc_new_block
    #endif /* ENABLE_GC */  //   concerning conservative computation.
  }

  rp->a = (int *)(&(rp->fp)->k.i); /* beginning of klump in first page */
  rp->b = (int *)((rp->fp)+1);     /* end of klump in first page */

  if ( rp->lobjs )
    {                                // free large objects
      Lobjs* lobjs = rp->lobjs;
      while ( lobjs ) 
	{
	  Lobjs* lobjsTmp = lobjs->next;
#ifdef ENABLE_GC
	  lobj_current -= size_lobj(lobjs->value);
#endif	  
	  free(lobjs);
	  lobjs = lobjsTmp;
	}
      rp->lobjs = NULL;
    }

#ifdef PROFILING
  rp->allocNow = 0;
  rp->allocProfNow = 0;
#endif

  debug(printf("]\n"));

  return rAdr; /* We preserve rAdr and the status bits. */
}

/*-------------------------------------------------------------------------*
 *deallocateRegionsUntil:                                                  *
 *  It is called with rAddr=sp, which do not nessesaraly point at a region *
 *  description. It deallocates all regions that are placed over sp.       *
 *  The function does not return or alter anything.                        *
 *-------------------------------------------------------------------------*/
void deallocateRegionsUntil(int rAddr
#ifdef KAM
			    , Ro** topRegionCell
#endif
			    ) { 
  Ro *rp;

  // debug(printf("[deallocateRegionsUntil(rAddr = %x, topFiniteRegion = %x)...\n", rAddr, topFiniteRegion));

  rp = (Ro *) clearStatusBits(rAddr);
  
#ifdef PROFILING
  callsOfDeallocateRegionsUntil++;
  while ((FiniteRegionDesc *)rp <= topFiniteRegion)
    {
      deallocRegionFiniteProfiling();
    }
#endif

  while (rp <= TOP_REGION) 
    { 
      /*printf("rp: %0x, top region %0x\n",rp,TOP_REGION);*/
      deallocateRegionNew(
#ifdef KAM
			  topRegionCell
#endif
			  );
    }

  debug(printf("]\n"));

  return;
} 

/*-------------------------------------------------------------------------*
 *deallocateRegionsUntil_X86: version of the above function working with   *
 *  the stack growing towards negative infinity.                           *
 *-------------------------------------------------------------------------*/
#ifndef KAM
void deallocateRegionsUntil_X86(int rAddr) { 
  Ro *rp;

  //  debug(printf("[deallocateRegionsUntil_X86(rAddr = %x, topFiniteRegion = %x)...\n", rAddr, topFiniteRegion));

  rp = (Ro *) clearStatusBits(rAddr);
  
#ifdef PROFILING
  callsOfDeallocateRegionsUntil++;

  /* Don't call deallocRegionFiniteProfiling if no finite 
   * regions are allocated. mael 2001-03-20 */
  while ( (topFiniteRegion != NULL) && (FiniteRegionDesc *)rp >= topFiniteRegion)
    {
      deallocRegionFiniteProfiling();
    }
#endif

  while (rp >= TOP_REGION) 
    {
      /*printf("rp: %0x, top region %0x\n",rp,TOP_REGION);*/
      deallocateRegionNew();
    }

  debug(printf("]\n"));

  return;
} 
#endif /* not KAM */



/*----------------------------------------------------------------*
 *        Profiling functions                                     *
 *----------------------------------------------------------------*/
#ifdef PROFILING

/***************************************************************************
 *     Changed runtime operations for making profiling possible.           *
 *                                                                         *
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
int *allocRegionInfiniteProfiling(Ro *roAddr, unsigned int regionId) { 
  Klump *kp;

  /* printf("[allocRegionInfiniteProfiling roAddr=%x, regionId=%d...", roAddr, regionId);*/

  callsOfAllocateRegionInf++;
  maxNoOfPages = max(++noOfPages, maxNoOfPages);
  profTabIncrNoOfPages(regionId, 1);
  profTabMaybeIncrMaxNoOfPages(regionId);
  regionDescUseInf += (sizeRo-sizeRoProf);
  maxRegionDescUseInf = max(maxRegionDescUseInf,regionDescUseInf);
  regionDescUseProfInf += sizeRoProf;
  maxRegionDescUseProfInf = max(maxRegionDescUseProfInf,regionDescUseProfInf);

  FREELIST_MUTEX_LOCK;

  if (freelist==NULL) callSbrk();

  #ifdef ENABLE_GC
  rp_used++;
  #endif /* ENABLE_GC */

  kp = freelist;
  freelist= freelist->k.n;

  FREELIST_MUTEX_UNLOCK;

  kp->k.n = NULL;                  // First and last region page
  kp->k.r = roAddr;                // Pointer back to region descriptor (used by GC)

  roAddr->a = (int *)(&(kp->k.i)); // We allocate from k.i in the page
  roAddr->b = (int *)(kp+1);       // The border is after this page
  roAddr->p = TOP_REGION;	   // Push this region onto the region stack
  roAddr->fp = kp;                 // Update pointer to the first page
  roAddr->allocNow = 0;            // No allocation yet
  roAddr->allocProfNow = 0;        // No allocation yet
  roAddr->regionId = regionId;     // Put name of region in region descriptor

  roAddr->lobjs = NULL;            // The list of large objects is empty

  TOP_REGION = roAddr;

  /* We have to set the infinitebit. */
  roAddr = (Ro *) setInfiniteBit((int) roAddr);

  debug(printf("exiting]\n"));

  return (int *) roAddr;
}

/* In CodeGenX86, we use a generic function to compile a C-call. The regionId */
/* may therefore be tagged, which this stub-function takes care of.           */
int *allocRegionInfiniteProfilingMaybeUnTag(Ro *roAddr, unsigned int regionId) { 
  return allocRegionInfiniteProfiling(roAddr, convertIntToC(regionId));
}

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
void allocRegionFiniteProfiling(FiniteRegionDesc *rdAddr, unsigned int regionId, int size) { 
  ObjectDesc *objPtr;  
  ProfTabList* p;
  int index;
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

  debug(printf("exiting, topFiniteRegion = %x, topFiniteRegion->p = %x, &topFiniteRegion = %x]\n", 
  	       topFiniteRegion, topFiniteRegion->p, &topFiniteRegion));

  return;
}

/* In CodeGenX86, we use a generic function to compile a C-call. The regionId */
/* and size may therefore be tagged, which this stub-function takes care of.  */
void allocRegionFiniteProfilingMaybeUnTag(FiniteRegionDesc *rdAddr, unsigned int regionId, int size) { 
  return allocRegionFiniteProfiling(rdAddr, convertIntToC(regionId), convertIntToC(size));
}

/*-----------------------------------------------------------------*
 * deallocRegionFiniteProfiling:                                   *
 * topFiniteRegion has to point at the bottom address of the       *
 * finite region descriptor, which will be the new stack address.  *
 *-----------------------------------------------------------------*/
int *deallocRegionFiniteProfiling(void) { 
  int size;

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

  debug(printf("exiting, topFiniteRegion = %x]\n", topFiniteRegion));
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
int *allocProfiling(int rAddr,int n, int pPoint) {
  int *res;

  debug(printf("[Entering allocProfiling... rAddr:%x, n:%d, pp:%d.", rAddr, n, pPoint));

  res = alloc(rAddr, n+sizeObjectDesc);   // allocate object descriptor and object
  
  ((ObjectDesc *)res)->atId = pPoint;     // initialize object descriptor
  ((ObjectDesc *)res)->size = n;
  
  res = (int *)(((ObjectDesc *)res) + 1); // return pointer to user data

  debug(printf("exiting]\n"));
  return res;
}

#endif /*PROFILING*/





