/*----------------------------------------------------------------*
 *                        Regions                                 *
 *----------------------------------------------------------------*/
#include "Flags.h"
#include "Region.h"
#include "Math.h"

/*----------------------------------------------------------------*
 * Global declarations                                            *
 *----------------------------------------------------------------*/
Klump * freelist;
Ro * topRegion;

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
             maxNoOfInstances=0,
             noOfInstances=0,
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

             maxProfStack=0;            /* At time of max. stack size, how much is due to profiling.        */
                                        /* It is updated in function Profiling.updateMaxProfStack, which is */
                                        /* called from the assembler file.                                  */
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


/* Print the structure of each region in the region stack. */
void printRegionStack(char *str) {
  Ro *rp;

  printf("Called from: %s\n",str);

#if PRINT_REGION_STACK_WHEN_DEBUG
  for (rp=topRegion;rp!=NULL;rp=rp->p)
    printf("Region at address: %0x, fp: %0x, b: %0x, a: %0x, p: %0x.\n",rp,(rp->fp),(rp->b),(rp->a),(rp->p));
#endif

  return;
}


#if DEBUG_ALLOC
/* functions trace and traceFreelist can be used for
   checking whether the freelist has been corrupted.

int trace(){
  /*  if ((callsOfAlloc >= 100000) && (callsOfAlloc % 1000 == 0)) */
  /*  if (callsOfAlloc > 150000)
   {if (callsOfAlloc % 1000 == 0)
      return 1;
   }
  return 0; */
 if (callsOfAlloc % 5000 == 0)
    return 1;
 else return 0;
}
  
void traceFreelist(int pPoint){
  Klump *np = freelist;
  int i = 0;
  if (trace() == 1) {
     printf("Tracing freelist at program point pp%d at  call number  %d of alloc...\n",pPoint, callsOfAlloc);
     while (np != NULL) {
       np = np->k.n;
       i++;
     }
     printf("... found %d region pages.\n", i);
  }
}
#endif

/* Print info about a region. */
void printRegionInfo(int rAddr,  char *str) {
  Ro *rp;

  rp = (Ro *) clearStatusBits(rAddr);
  printf("printRegionInfo called from: %s\n",str);
  printf("Region at address: %0x\n", rp);
  printf("  fp: %0x\n", (rp->fp));
  printf("  b : %0x\n", (rp->b));
  printf("  a : %0x\n", (rp->a));
  printf("  p : %0x\n", (rp->p));
  return;
}

/* Calculate number of pages in an infinite region. */
int NoOfPagesInRegion(Ro *rp) {
  int i;
  Klump *ptr;

  for (i=0,ptr=rp->fp;ptr!=NULL;ptr=ptr->k.n)
    i++;

  return i;
}

/*-------------------------------------------------------------------------*
 *                         Region operations.                              *
 *                                                                         *
 * allocateRegion: Allocates a region and return a pointer to it.          *
 * allocateRegionFast: Allocates a region and return a pointer to it.      *
 * deallocateRegion: Pops the top region of the region stack.              *
 * callSbrk: Updates the freelist with new region pages.                   *
 * new_page: Returns a new page, and call sbrk if nessesary.               *
 * alloc: Allocates n words in a region.                                   *
 * resetRegion: Resets a region by freeing all pages except one            *
 * deallocateRegionsUntil: Alle regions over a thresehold are deallocated. *
 *-------------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
 *allocateRegionFast:                                                   *
 *  The function do not allocate a first regionpage for the region.     *
 *  Put a region administration structure on the stack. The address are *
 *  in roAddr.                                                          *
 *----------------------------------------------------------------------*/
int *allocateRegionFast(Ro *roAddr)
{ 

#if DEBUG_ALLOCATE_REGION
  printRegionStack("allocateRegion-ENTER");
#endif

  roAddr->a = NULL;
  roAddr->b = NULL;
  roAddr->p = topRegion;
  roAddr->fp = NULL;
  topRegion = roAddr;

  /* We have to set the infinitebit. */
  roAddr = (Ro *) setInfiniteBit((int) roAddr);

#if DEBUG_ALLOCATE_REGION
  printRegionStack("allocateRegion-LEAVE");
#endif

  return (int *) roAddr;
}  

/*----------------------------------------------------------------------*
 *allocateRegion:                                                       *
 *  Get a first regionpage for the region.                              *
 *  Put a region administrationsstructure on the stack. The address are *
 *  in roAddr.                                                          *
 *----------------------------------------------------------------------*/
int *allocateRegion(Ro *roAddr)
{ Klump *kp;
  extern Klump * freelist;

#if DEBUG_ALLOCATE_REGION
  printRegionStack("allocateRegion-ENTER");
#endif
  
  if (freelist==NULL)
    callSbrk();

  kp = freelist;
  freelist= freelist->k.n;
  kp->k.n = NULL;

  roAddr->a = (int *)(&(kp->k.i)); /* We allocate from k.i in the page. */ 
  roAddr->b = (int *)(kp+1);       /* The border is after this page. */
  roAddr->p = topRegion;	   /* Push this region onto the region stack. */
  roAddr->fp = kp;                 /* Update pointer to the first page. */
  topRegion = roAddr;

  /* We have to set the infinitebit. */
  roAddr = (Ro *) setInfiniteBit((int) roAddr);

#if DEBUG_ALLOCATE_REGION
  printRegionStack("allocateRegion-LEAVE");
#endif

  return (int *) roAddr;
}  

/*----------------------------------------------------------------------*
 *deallocateRegion:                                                     *
 *  Pops the top region of the stack, and insert the regionpages in the *
 *  free list. There have to be atleast one region on the stack.        *
 *  When profiling we also use this function.                           *
 *----------------------------------------------------------------------*/
int *deallocateRegion()
{ int *sp;
  extern Klump * freelist;
  extern Ro * topRegion;

#ifdef PROFILING
  int i;
  callsOfDeallocateRegionInf++;
  regionDescUseInf -= (sizeRo-sizeRoProf);
  regionDescUseProfInf -= sizeRoProf;
#if DEBUG_DEALLOCATE_REGION
  printRegionStack("deallocateRegion-ENTER");
  printf("\nNu deallokeres region paa adresse %0x.\n",topRegion);
#endif
  i = NoOfPagesInRegion(topRegion);
  noOfPages -= i;
  noOfInstances --;
  allocNowInf -= topRegion->allocNow;
  allocProfNowInf -= topRegion->allocProfNow;
  if (topRegion->regionId > MAX_REGIONS_TO_PROFILE) {
    printf("RegionId %d too large in DeallocateRegion. Change constant MAX_REGIONS_TO_PROFILE in file Runtime.h\n", topRegion->regionId);
    exit(-1);
  }
  profTabSetNoOfPages(topRegion->regionId, profTabGetNoOfPages(topRegion->regionId) - i);
  profTabSetNoOfInstances(topRegion->regionId, profTabGetNoOfInstances(topRegion->regionId) - 1);
  profTabSetAllocNow(topRegion->regionId, profTabGetAllocNow(topRegion->regionId) - (topRegion->allocNow));
#endif

  sp = (int *) topRegion; /* topRegion points at the bottom of the region description on the stack. */

  if (topRegion->fp) { /* Insert the region pages in the freelist. */
    (((Klump *)topRegion->b)-1)->k.n = freelist;
    freelist = topRegion->fp;
  }
  topRegion=topRegion->p;

#if DEBUG_DEALLOCATE_REGION
  printRegionStack("deallocateRegion-LEAVE");
#endif

  return sp;
}

/*----------------------------------------------------------------------*
 *callSbrk:                                                             *
 *  Sbrk is called and the free list is updated.                        *
 *  The free list has to be empty.                                      *
 *----------------------------------------------------------------------*/
void callSbrk()
{ Klump *np;
  extern Klump * freelist;
  char *sb;
  int temp;

#ifdef PROFILING
    callsOfSbrk++;
#endif

  /* We must manually insure double alignment. Some operating systems (like *
   * HP UX) does not return a double aligned address...                     */

    /*printf("callSbrk: calling malloc\n");*/
  sb = (char *)malloc(BYTES_ALLOC_BY_SBRK + 8);
  /*printf("callSbrk: return from malloc\n");*/
  if (sb == (char *)-1) {
    perror("SBRK error\n");
    exit(-1);
  }

  #if DEBUG_SBRK
  printf("callSbrk : sb mod 8 = %d\n", ((int)sb) % 8);
  #endif

  /* alignment (martin) */
  if (temp=((int)sb % 8)) {
    sb = (char *) (((int)sb) + 8 - temp);
  }

  #if DEBUG_SBRK
  if ((int)sb % 8) {
    printf("callSbrk returns unaligned sb. sb mod 8 = %d\n", ((int)sb) % 8);
    exit(-1);
  }
  #endif

  np = (Klump *) sb;
  freelist = np;
  /* We have to fragment the SBRK-chunk into region pages. */
  while ((char *)(np+1) < ((char *)freelist)+BYTES_ALLOC_BY_SBRK) { 
    np++;
    (np-1)->k.n = np;
  }
  np->k.n = NULL;
}

/*----------------------------------------------------------------------*
 *new_page:                                                             *
 *  Return a region page from the free list. If the free list is empty  *
 *  sbrk is called and a new free list is created.                      *
 *----------------------------------------------------------------------*/
Klump *new_page()
{ Klump *kp;
  extern Klump * freelist;
  char *sb;

  if (freelist==NULL) {
    callSbrk();
  }
  kp = freelist;
  freelist= freelist->k.n;
  kp->k.n = NULL;
  return kp;
}

/*----------------------------------------------------------------------*
 *alloc_new_block:                                                      *
 *  Allocates a new block in region.                                    *
 *----------------------------------------------------------------------*/

void alloc_new_block(Ro *rp)
{ Klump* np;
  extern Klump * freelist;

#ifdef PROFILING
  /*  if (rp->regionId == 80022) {
     printf("alloc_new_block: enter with rp :%d\n", rp); 
     
  } */
#endif
#ifdef PROFILING
  if (profTabGetNoOfPages(rp->regionId) >= profTabGetMaxNoOfPages(rp->regionId))
    profTabSetMaxNoOfPages(rp->regionId, profTabGetNoOfPages(rp->regionId)+1); 
  maxNoOfPages = max(++noOfPages, maxNoOfPages);
#endif

#ifdef PROFILING
  if (rp->regionId  ==  80022) {
     printf("alloc_new_block(1): freelist :%0x\n", freelist);
     
  } 
#endif
  if (freelist==NULL)
    callSbrk();
 
#ifdef PROFILING
    /*  if (rp->regionId == 80022) {
     printf("alloc_new_block(2a): freelist :%0x\n", freelist);
     printf("alloc_new_block(2b): freelist->k.n :%0x\n", freelist->k.n); 
     
  } */
#endif
  np = freelist;
  freelist= freelist->k.n;
  np->k.n = NULL;

  if (rp->fp)
    (((Klump *)(rp->b))-1)->k.n = np; /* Updates the next field in the last region page. */
  else
    rp->fp = np;                    /* Update pointer to the first page. */

  rp->a = (int *)(&(np->k.i));    /* Updates the allocation pointer. */
  rp->b = (int *)(np+1);          /* Updates the border pointer. */

}

/*----------------------------------------------------------------------*
 *alloc:                                                                *
 *  Allocates n words in region rAddr. If the CHECKREGION flag is set,  *
 *  it will make sure, that there are space for the n words before      *
 *  doing the allocation.                                               *
 *  Pre-condition: n <= ALLOCATABLE_WORDS_IN_REGION_PAGE                *
 *----------------------------------------------------------------------*/
int *alloc (int rAddr, int n)
{ int *t1;
  int *t2;
  int *t3;
  Ro *rp;

  #ifdef PROFILING
    int *i;
    int j;
  #endif

#if DEBUG_ALLOC
  printf("alloc-ENTER rAddr = %d, n = %d\n", rAddr, n);
  /* printRegionStack("alloc-ENTER");*/
#endif

  rp = (Ro *) clearStatusBits(rAddr);


#ifdef PROFILING
#if DEBUG_ALLOC
    printf("Region descriptor at address (enter): %0x, fp: %0x, b: %0x, a: %0x, p: %0x, allocNow: %d, allocProfNow: %d, regionId: %d.\n",
    rp,(rp->fp),(rp->b),(rp->a),(rp->p), (rp->allocNow),(rp->allocProfNow),(rp->regionId));
#endif
  callsOfAlloc++;
  if (n-sizeObjectDesc < 0) {
    printf("ERROR -- alloc with profiling\n");
    exit(-1);
  }
  allocNowInf += n-sizeObjectDesc; /* When profiling we also allocate an object descriptor. */
  maxAllocInf = max(allocNowInf, maxAllocInf);
  maxAlloc = max(maxAlloc, allocNowInf+allocNowFin);
  rp->allocNow += n-sizeObjectDesc;
  allocProfNowInf += sizeObjectDesc;
  if (maxAllocInf == allocNowInf)
    maxAllocProfInf = allocProfNowInf;
  rp->allocProfNow += sizeObjectDesc;
  if (rp->regionId >= MAX_REGIONS_TO_PROFILE) {
    printf("RegionId %d too large in alloc(%d). Change constant MAX_REGIONS_TO_PROFILE, currently with value %d, in file Runtime.h\n", rp->regionId, n,MAX_REGIONS_TO_PROFILE);
    exit(-1);
  }
  profTabSetAllocNow(rp->regionId, profTabGetAllocNow(rp->regionId) + (n-sizeObjectDesc));
  if (profTabGetAllocNow(rp->regionId) > profTabGetMaxAlloc(rp->regionId))
    profTabSetMaxAlloc(rp->regionId, profTabGetAllocNow(rp->regionId));

#if DEBUG_ALLOC
    printf("Region descriptor at address (leave): %0x, fp: %0x, b: %0x, a: %0x, p: %0x, allocNow: %d, allocProfNow: %d, regionId: %d.\n",
    rp,(rp->fp),(rp->b),(rp->a),(rp->p), (rp->allocNow),(rp->allocProfNow),(rp->regionId));
#endif
#endif

  t1 = rp->a;
  t2 = t1 + n;

#if CHECKREGION
  t3 = rp->b;
  if (t2 > t3) {
#if DEBUG_ALLOC
    printf("calling       alloc_new_block \n");
#endif
    alloc_new_block(rp);
#if DEBUG_ALLOC
    printf("returned from alloc_new_block \n");
#endif

    #ifdef PROFILING
      j=0;
      for (i=rp->a; i<rp->b;) { /* Put zeroes in region page. */
        *i = notPP;
	i++;
	j++;
      }
      if (j != ALLOCATABLE_WORDS_IN_REGION_PAGE)
	printf("ERROR - allocProfiling, when putting zeroes in region page, j %d and rpSize %d\n",
	       j, ALLOCATABLE_WORDS_IN_REGION_PAGE);
    #endif

    t1 = rp->a;
    t2 = t1+n;
  }
#endif

  rp->a = t2;

#if DEBUG_ALLOC
  printRegionStack("alloc-LEAVE");
#endif

  return t1;
}

/*----------------------------------------------------------------------*
 *resetRegion:                                                          *
 *  All regionpages except one are inserted into the free list, and     *
 *  the region administration structure is updated. The statusbits are  *
 *  not changed.                                                        *
 *----------------------------------------------------------------------*/
void resetRegion(int rAdr)
{ Ro *rp;

  extern Klump * freelist;

  #ifdef PROFILING
    int *i;
    Klump *temp;
    int j;
    int *zerolimit;
  #endif

  rp = (Ro *) clearStatusBits(rAdr);

#if DEBUG_RESET_REGION
  printRegionStack("resetRegion-ENTER");
#ifdef PROFILING
  printf("\nNu resettes region paa adresse %0x med id %d.\n",rp, rp->regionId);
#else
  printf("\nNu resettes region paa adresse %0x.\n",rp);
#endif
#endif

#ifdef PROFILING
  callsOfResetRegion++;
  j = NoOfPagesInRegion(rp);
  if (rp->regionId > MAX_REGIONS_TO_PROFILE) {
    printf("RegionId %d too large in resetRegion. Change constant MAX_REGIONS_TO_PROFILE, currently with value %d, in file Runtime.h\n", rp->regionId, MAX_REGIONS_TO_PROFILE);
    exit(-1);
  }
  if (j) { /* We have to check that there are at least one page. */
    noOfPages -= j-1;
    profTabSetNoOfPages(rp->regionId, profTabGetNoOfPages(rp->regionId) - (j-1));
  }
  allocNowInf -= rp->allocNow;
  profTabSetAllocNow(rp->regionId, profTabGetAllocNow(rp->regionId) - (rp->allocNow));
  allocProfNowInf -= rp->allocProfNow;
#endif

  if (rp->fp) { /* There are at least one page in the region. */
    if ( (rp->fp)->k.n != NULL ) { /* There are more than one page in the region. */
      (((Klump *)rp->b)-1)->k.n = freelist;
      freelist = (rp->fp)->k.n;
      (rp->fp)->k.n = NULL;
    }
  #ifdef PROFILING
    if (j==1){
       /* only put zeroes in that part of the page that contains values */
       zerolimit = rp->a;
     }
    else
       /* put zeroes in the entire first page */
       zerolimit = (int *)((rp->fp)+1);
  #endif
       
    rp->a = (int *)(&(rp->fp)->k.i); /* beginning of klump in first page */
    rp->b = (int *)((rp->fp)+1);     /* end of klump in first page */

    #ifdef PROFILING
      rp->allocNow = 0;
      rp->allocProfNow = 0;
      for (i=rp->a; i<zerolimit;) { /* Put zeroes in region page. */
	*i = notPP;
	i++;
      }
    #endif

  }

#if DEBUG_RESET_REGION
  printRegionStack("resetRegion-LEAVE");
#endif

  return;
}

/*-------------------------------------------------------------------------*
 *deallocateRegionsUntil:                                                  *
 *  It is called with rAddr=sp, which do not nessesaraly point to a region *
 *  description. It deallocates all regions that are placed over sp.       *
 *  The function do not return or alter anything.                          *
 *-------------------------------------------------------------------------*/
void deallocateRegionsUntil(int rAddr)
{ Ro *rp;
  extern Ro * topRegion;

#if DEBUG_DEALLOCATE_REGIONS_UNTIL
  printRegionStack("deallocateRegionsUntil-ENTER");
#endif

  rp = (Ro *) clearStatusBits(rAddr);
  
#ifdef PROFILING
  callsOfDeallocateRegionsUntil++;
#endif

  #ifdef PROFILING
    while ((FiniteRegionDesc *)rp <= topFiniteRegion) {
      deallocRegionFiniteProfiling();
    }
  #endif

  while (rp <= topRegion)
    deallocateRegion();

#if DEBUG_DEALLOCATE_REGIONS_UNTIL
  printRegionStack("deallocateRegionsUntil-LEAVE");
#endif

  return;
} 


/*----------------------------------------------------------------*
 *        Profiling functions                                     *
 *----------------------------------------------------------------*/
#ifdef PROFILING

/***************************************************************************
 *     Changed runtime operations for making profiling possible.           *
 *                                                                         *
 *                                                                         *
 * allocateRegionProfiling(roAddr, regionId)                               *
 * allocRegionFiniteProfiling(rdAddr, regionId, size)                      *
 * deallocRegionFiniteProfiling(void)                                      *
 * allocProfiling(rAddr, n, pPoint)                                        *
 * storePrgPointProfiling(pPoint, *objPtr)                                 *
 * updateSizeForDoublesProfiling(size, *doublePtr)                         *
 ***************************************************************************/

/*----------------------------------------------------------------------*
 *allocRegionInfiniteProfiling:                                         *
 *  Get a first regionpage for the region.                              *
 *  Put a region administration structure on the stack. The address are *
 *  in roAddr. The name of the region is regionId                       *
 *  There has to be room for the region descriptor on the stack, which  *
 *  roAddr points at.                                                   *
 *----------------------------------------------------------------------*/
int *allocRegionInfiniteProfiling(Ro *roAddr, unsigned int regionId)
{ Klump *kp;
  extern Klump * freelist;
  int *i;
  int j;

#if DEBUG_ALLOCATE_REGION
  printRegionStack("allocateRegion-ENTER");
  printf("Allocating region: %d\n", regionId);
#endif
  
  callsOfAllocateRegionInf++;
  maxNoOfPages = max(++noOfPages, maxNoOfPages);
  if (profTabGetNoOfPages(regionId) >= profTabGetMaxNoOfPages(regionId))
    profTabSetMaxNoOfPages(regionId, profTabGetNoOfPages(regionId) + 1); 
  maxNoOfInstances = max(++noOfInstances, maxNoOfInstances);
  if (profTabGetNoOfInstances(regionId) >= profTabGetMaxNoOfInstances(regionId))
    profTabSetMaxNoOfInstances(regionId, profTabGetNoOfInstances(regionId) + 1); 

  regionDescUseInf += (sizeRo-sizeRoProf);
  maxRegionDescUseInf = max(maxRegionDescUseInf,regionDescUseInf);
  regionDescUseProfInf += sizeRoProf;
  maxRegionDescUseProfInf = max(maxRegionDescUseProfInf,regionDescUseProfInf);

  if (freelist==NULL)
    callSbrk();

  kp = freelist;
  freelist= freelist->k.n;
  kp->k.n = NULL;

  roAddr->a = (int *)(&(kp->k.i)); /* We allocate from k.i in the page. */ 
  roAddr->b = (int *)(kp+1);       /* The border is after this page. */
  roAddr->p = topRegion;	   /* Push this region onto the region stack. */
  roAddr->fp = kp;                 /* Update pointer to the first page. */
  roAddr->allocNow = 0;            /* No allocation yet. */
  roAddr->allocProfNow = 0;        /* No allocation yet. */
  roAddr->regionId = regionId;     /* Put name of region in region descriptor. */
  topRegion = roAddr;

  j=0;
  for (i=roAddr->a; i<roAddr->b;) { /* Put zeroes in region page. */
    *i = notPP;
    i++;
    j++;
  }
  if (j != ALLOCATABLE_WORDS_IN_REGION_PAGE) {
    sprintf(errorStr, "ERROR - allocProfiling, when putting zeroes in region page, j %d and rpSize %d\n",
	    j, ALLOCATABLE_WORDS_IN_REGION_PAGE);
    printERROR(errorStr);
  }

  /* We have to set the infinitebit. */
  roAddr = (Ro *) setInfiniteBit((int) roAddr);

#if DEBUG_ALLOCATE_REGION
  printRegionStack("allocateRegion-LEAVE");
#endif

  return (int *) roAddr;
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
void allocRegionFiniteProfiling(FiniteRegionDesc *rdAddr, unsigned int regionId, int size)
{ ObjectDesc *objPtr;  

#if DEBUG_ALLOC_REGION_FINITE_PROFILING
  printf("allocRegionFiniteProfiling --- ENTER   top: %d rdAddr: %d regionId: %d size: %d\n", topFiniteRegion, rdAddr, regionId, size);
#endif


  callsOfAllocateRegionFin++;
  maxNoOfInstances = max(++noOfInstances, maxNoOfInstances);
  if (regionId > MAX_REGIONS_TO_PROFILE) {
    printf("RegionId %d too large in allocRegionFiniteProfiling. Change constant MAX_REGIONS_TO_PROFILE, currently with value %d, in file Runtime.h\n", regionId, MAX_REGIONS_TO_PROFILE);
    exit(-1);
  }
  if (profTabGetNoOfInstances(regionId) >= profTabGetMaxNoOfInstances(regionId))
    profTabSetMaxNoOfInstances(regionId, profTabGetNoOfInstances(regionId) + 1);

  allocNowFin += size; 
  maxAllocFin = max(allocNowFin, maxAllocFin);
  maxAlloc = max(maxAlloc, allocNowFin+allocNowInf);
  allocProfNowFin += sizeObjectDesc;
  regionDescUseProfFin += sizeFiniteRegionDesc;
  if (allocNowFin == maxAllocFin) {
    maxAllocProfFin = allocProfNowFin;
    maxRegionDescUseProfFin = regionDescUseProfFin;
  }
  profTabSetAllocNow(regionId, profTabGetAllocNow(regionId) + size);
  if (profTabGetAllocNow(regionId) > profTabGetMaxAlloc(regionId))
    profTabSetMaxAlloc(regionId, profTabGetAllocNow(regionId));

  rdAddr->p = topFiniteRegion; /* link to previous region description on stack */
  rdAddr->regionId = regionId; /* Put name on region in descriptor. */
  topFiniteRegion = (FiniteRegionDesc *)(rdAddr); /* pointer to topmost region description on stack */

  objPtr = (ObjectDesc *)(rdAddr + 1); /* We also put the object descriptor onto the stack. */
  objPtr->atId = notPrgPoint;
  objPtr->size = size;

#if DEBUG_ALLOC_REGION_FINITE_PROFILING
  printf("allocRegionFiniteProfiling --- LEAVE   top: %d\n", topFiniteRegion);
#endif
  return;
}

/*-----------------------------------------------------------------*
 * deallocRegionFiniteProfiling:                                   *
 * topFiniteRegion has to point at the bottom address of the       *
 * finite region descriptor, which will be the new stack address.  *
 *-----------------------------------------------------------------*/
int *deallocRegionFiniteProfiling(void)
{ int *sp;

#if DEBUG_DEALLOC_REGION_FINITE_PROFILING
  printf("deallocRegionFiniteProfiling --- ENTER top: %d\n", topFiniteRegion);
#endif

  callsOfDeallocateRegionFin++;
  noOfInstances--;
  if (topFiniteRegion->regionId > MAX_REGIONS_TO_PROFILE) {
    printf("RegionId %d too large in deallocRegionFiniteProfiling. Change constant MAX_REGIONS_TO_PROFILE, currently with value %d, in file Runtime.h\n", topFiniteRegion->regionId, MAX_REGIONS_TO_PROFILE);
    exit(-1);
  }
  profTabSetNoOfInstances(topFiniteRegion->regionId, profTabGetNoOfInstances(topFiniteRegion->regionId) - 1);
    
  allocNowFin -= ((ObjectDesc *) (topFiniteRegion + 1))->size; 
  profTabSetAllocNow(topFiniteRegion->regionId, profTabGetAllocNow(topFiniteRegion->regionId) 
		     - (((ObjectDesc *) (topFiniteRegion + 1))->size));

  allocProfNowFin -= sizeObjectDesc;
  regionDescUseProfFin -= sizeFiniteRegionDesc;

  sp = (int *) topFiniteRegion;
  topFiniteRegion = topFiniteRegion->p; /* pop ptr. to prev. region desc. */

#if DEBUG_DEALLOC_REGION_FINITE_PROFILING
  printf("deallocRegionFiniteProfiling --- LEAVE top: %d\n", topFiniteRegion);
#endif

  return sp;
}


/*-----------------------------------------------------------------*
 * allocProfiling:                                                 *
 * Same as alloc, except that an object descriptor is created.     *
 * In particular, n is still the number of words requested for     *
 * user values (not including the object descriptor).  However,    *
 * allocProfiling asks alloc for space for the object descriptor   *
 * and takes care of allocating it, returning a pointer to the     *
 * beginning of the user value, as though profiling were not on.   *
 *                                                                 *
 * Precondition:                                                   *
 *     n <= ALLOCATABLE_WORDS_IN_REGION_PAGE - sizeObjectDescc     *
 *                                                                 *
 *-----------------------------------------------------------------*/
int *allocProfiling(int rAddr,int n, int pPoint) {
  int *res;
  int *i;

#if DEBUG_ALLOC
  printf("allocProfiling ENTER at program point %d\n", pPoint);
#ifdef PROFILING
  traceFreelist(pPoint);
#endif
#endif


  if (pPoint >= MAX_ALLOCATION_POINTS || pPoint < 1) {
    sprintf(errorStr, "ERROR -- allocProfiling is called with program point: %d\nwhich exceeds %d, the upper limit of program points currentely\nreserved by the profiler. To solve the problem,\nincrease the value of MAX_ALLOCATION_POINTS (in Region.h) to\nthe number of program points you need and then recompile\nthe runtime system.\n",
	    pPoint,MAX_ALLOCATION_POINTS);
    printERROR(errorStr);
  }

  if (n + sizeObjectDesc> ALLOCATABLE_WORDS_IN_REGION_PAGE) {
    sprintf(errorStr, "ERROR -- allocProfiling is called with request for object of size %d words(not including the size of the object descriptor) and ALLOCATABLE_WORDS_IN_REGION_PAGE is only %d\nTo solve the problem,\nincrease the value of ALLOCATABLE_WORDS_IN_REGION_PAGE (in Region.h) to\nthe number of words you need and then recompile\nthe runtime system.\n",
	    n, ALLOCATABLE_WORDS_IN_REGION_PAGE);
    printERROR(errorStr);
  }


  /* allocate object descriptor and object */
  res = alloc(rAddr, n+sizeObjectDesc);
  /* initialize object descriptor */
  ((ObjectDesc *)res)->atId = pPoint;
  ((ObjectDesc *)res)->size = n;
  /* return pointer to user data */
  res = (int *)(((ObjectDesc *)res) + 1);

#if DEBUG_PROFILE_TICK 
  {Ro *rpTmp;
   rpTmp = (Ro *) clearStatusBits(rAddr);
   printf("Allocates object of size %d with prgPoint %d in region %d\n",n,pPoint,rpTmp->regionId);
   check_infinite_regions();
  } 
#endif

#if DEBUG_ALLOC
  printf("allocProifiling LEAVE program point %d\n", pPoint);
#endif
  return res;
}


/*------------------------------------------------------------------*
 * storePrgPointProfiling:                                          *
 * objPtr is pointing at the object, and not the object descriptor. *
 * We could make an test on the size field each time pp i set.      *
 *------------------------------------------------------------------*/
void storePrgPointProfiling(int pPoint, int *objPtr) {

#if DEBUG_STORE_PRG_POINT
  /*printf("Enter to storePrgPointProfiling\n");*/
  if (pPoint < 2 || pPoint > MAX_ALLOCATION_POINTS) {
    printf("We have a program point too small or too big: %d\n", pPoint);
  }
#endif

  if ( (((ObjectDesc *)objPtr)-1)->size == dummyDouble ) {
    /* We have an unaligned double in a finite region.      */
    /* The object descriptor is therefore one address lower */
    /* than usual.                                          */
   
#if DEBUG_STORE_PRG_POINT
    printf("We have an not aligned double with atId %d, size %d, dummyReal %d and tagDouble %d.\n",
	   *(objPtr-3), *(objPtr-2), *(objPtr-1), *(objPtr));
    if ((((ObjectDesc *) (((int *)objPtr)-1))-1)->atId != 1)
      printf("Update program point %d\n", (((ObjectDesc *) (((int *)objPtr)-1))-1)->atId);
#endif

    (((ObjectDesc *) (((int *)objPtr)-1))-1)->atId = pPoint;
  }
  else {
#if DEBUG_STORE_PRG_POINT
    if ((((ObjectDesc *)objPtr)-1)->atId != 1) 
      printf("Update program point %d\n", (((ObjectDesc *)objPtr)-1)->atId);
#endif
    (((ObjectDesc *)objPtr)-1)->atId = pPoint;
  }

#if DEBUG_STORE_PRG_POINT
  /*printf("Exit from storePrgPointProfiling\n");*/
#endif

  return;
}

/*------------------------------------------------------------------*
 * updateSizeForDoublesProfiling:                                   *
 * doublePtr is used to find the finite region descriptor so that   *
 * the regionId is known. That id is then used to update profTab    *
 * information. This function is needed because the finite region   *
 * is allocated with size 0 and the size field is first updated in  *
 * allocDoubleProfiling and hence profTab information is not        *
 * correct before this update. This function is called from         *
 * allocDoubleProfiling in KbpToHpPa; first time the size is known. *
 *------------------------------------------------------------------*/
int *updateSizeForDoublesProfiling(int size, int *doublePtr) {
  int regionId;
  FiniteRegionDesc *frAddr;

  if ( (((ObjectDesc *)doublePtr)-1)->size == dummyDouble ) {
    /* We have an unaligned double in a finite region.      */
    /* The object descriptor is therefore one address lower */
    /* than usual.                                          */
   
#if DEBUG_UPDATE_SIZE_FOR_DOUBLES
    printf("We have an not aligned double with atId %d, size %d, dummyReal %d and tagDouble %d.\n",
	   *(doublePtr-3), *(doublePtr-2), *(doublePtr-1), *(doublePtr));
#endif

    frAddr = ((FiniteRegionDesc *) (((ObjectDesc *) (((int *)doublePtr)-1))-1))-1;
  }
  else
    frAddr = ((FiniteRegionDesc *) (((ObjectDesc *)doublePtr)-1))-1;
  
  regionId = frAddr->regionId;

  allocNowFin += size; 
  maxAllocFin = max(allocNowFin, maxAllocFin);
  maxAlloc = max(maxAlloc, allocNowFin+allocNowInf);
  if (regionId > MAX_REGIONS_TO_PROFILE) {
    printf("RegionId %d too large in updateSizeForDoublesProfiling. Change constant MAX_REGIONS_TO_PROFILE, currently with value %d, in file Runtime.h\n", regionId, MAX_REGIONS_TO_PROFILE);
    exit(-1);
  }
  profTabSetAllocNow(regionId,profTabGetAllocNow(regionId) + size);
  if (profTabGetAllocNow(regionId) > profTabGetMaxAlloc(regionId))
    profTabSetMaxAlloc(regionId, profTabGetAllocNow(regionId));

  return doublePtr;

}

#endif /*PROFILING*/





