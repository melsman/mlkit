/*----------------------------------------------------------------*
 *                        Regions                                 *
 *----------------------------------------------------------------*/
#include "Flags.h"
#include "Region.h"
#include "Math.h"
#include "Profiling.h"

/*----------------------------------------------------------------*
 * Global declarations                                            *
 *----------------------------------------------------------------*/
Klump * freelist;
Ro * topRegion;

#ifdef PROFILING
extern int showStat;
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

/* Print info about a region. */
void printTopRegInfo() {
  Ro *rp;
  Klump *kp;

  rp = (Ro *) clearStatusBits((int) topRegion);
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


/* Print info about a region. */
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

/* Calculate number of pages in an infinite region. */
static int NoOfPagesInRegion(Ro *rp) {
  int i;
  Klump *ptr;
  for (i=0,ptr=rp->fp;ptr!=NULL;ptr=ptr->k.n) i++;
  return i;
}

void printFreeList() {
  Klump *kp;
  extern Klump * freeList;

  printf("Enter printFreeList\n");
  kp = freelist;
  while (kp != NULL) {
    printf(" %0x ",kp);
    kp = kp->k.n;
  }
  printf("Exit printFreeList\n");
  return;
}

/*-------------------------------------------------------------------------*
 *                         Region operations.                              *
 *                                                                         *
 * allocateRegion: Allocates a region and return a pointer to it.          *
 * deallocateRegion: Pops the top region of the region stack.              *
 * callSbrk: Updates the freelist with new region pages.                   *
 * alloc: Allocates n words in a region.                                   *
 * resetRegion: Resets a region by freeing all pages except one            *
 * deallocateRegionsUntil: All regions above a threshold are deallocated.  *
 *-------------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
 *allocateRegion:                                                       *
 *  Get a first regionpage for the region.                              *
 *  Put a region administrationsstructure on the stack. The address are *
 *  in roAddr.                                                          *
 *----------------------------------------------------------------------*/
int *allocateRegion(Ro *roAddr) { 
  Klump *kp;
  extern Klump * freelist;
  
  roAddr = (Ro *) clearStatusBits((int)roAddr);

  if (freelist==NULL) callSbrk();

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

  return (int *) roAddr;
}  

/*----------------------------------------------------------------------*
 *deallocateRegion:                                                     *
 *  Pops the top region of the stack, and insert the regionpages in the *
 *  free list. There have to be atleast one region on the stack.        *
 *  When profiling we also use this function.                           *
 *----------------------------------------------------------------------*/
int *deallocateRegion() { 
  int *sp;
  extern Klump * freelist;
  extern Ro * topRegion;

#ifdef PROFILING
  int i;
  callsOfDeallocateRegionInf++;
  regionDescUseInf -= (sizeRo-sizeRoProf);
  regionDescUseProfInf -= sizeRoProf;
  i = NoOfPagesInRegion(topRegion);
  noOfPages -= i;
  allocNowInf -= topRegion->allocNow;
  allocProfNowInf -= topRegion->allocProfNow;
  profTabDecrNoOfPages(topRegion->regionId, i);
  profTabDecrAllocNow(topRegion->regionId, topRegion->allocNow);
#endif

  sp = (int *) topRegion;   /* topRegion points at the bottom of the region 
			     * description on the stack. */

  /* Insert the region pages in the freelist; there is always 
   * at-least one page in a region. */
  (((Klump *)topRegion->b)-1)->k.n = freelist;
  freelist = topRegion->fp;
  topRegion=topRegion->p;

  return sp;
}

/*----------------------------------------------------------------------*
 * NEW NEW NEW NEW NEW                                                  *
 * We may not change sp!                                                *
 *deallocateRegion:                                                     *
 *  Pops the top region of the stack, and insert the regionpages in the *
 *  free list. There have to be atleast one region on the stack.        *
 *  When profiling we also use this function.                           *
 *----------------------------------------------------------------------*/
void deallocateRegionNew() { 
  extern Klump * freelist;
  extern Ro * topRegion;

#ifdef PROFILING
  int i;
  callsOfDeallocateRegionInf++;
  regionDescUseInf -= (sizeRo-sizeRoProf);
  regionDescUseProfInf -= sizeRoProf;
  i = NoOfPagesInRegion(topRegion);
  noOfPages -= i;
  allocNowInf -= topRegion->allocNow;
  allocProfNowInf -= topRegion->allocProfNow;
  profTabDecrNoOfPages(topRegion->regionId, i);
  profTabDecrAllocNow(topRegion->regionId, topRegion->allocNow);
#endif

  /* Insert the region pages in the freelist; there is always 
   * at-least one page in a region. */
  (((Klump *)topRegion->b)-1)->k.n = freelist;
  freelist = topRegion->fp;
  topRegion=topRegion->p;


  return;
}

/*----------------------------------------------------------------------*
 *callSbrk:                                                             *
 *  Sbrk is called and the free list is updated.                        *
 *  The free list has to be empty.                                      *
 *----------------------------------------------------------------------*/
void callSbrk() { 
  Klump *np;
  extern Klump * freelist;
  char *sb;
  int temp;

#ifdef PROFILING
  callsOfSbrk++;
#endif

  /* We must manually insure double alignment. Some operating systems (like *
   * HP UX) does not return a double aligned address...                     */

  sb = (char *)malloc(BYTES_ALLOC_BY_SBRK + 8);

  if (sb == (char *)NULL) {
    perror("I could not allocate more memory; either no more memory is\navailable or the memory subsystem is detectively corrupted\n");
    exit(-1);
  }

  /* alignment (martin) */
  if (temp=((int)sb % 8)) {
    sb = (char *) (((int)sb) + 8 - temp);
  }

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
 *alloc_new_block:                                                      *
 *  Allocates a new block in region.                                    *
 *----------------------------------------------------------------------*/

void alloc_new_block(Ro *rp) { 
  Klump* np;
  extern Klump * freelist;

#ifdef PROFILING
  profTabMaybeIncrMaxNoOfPages(rp->regionId);
  maxNoOfPages = max(++noOfPages, maxNoOfPages);
#endif

  if (freelist==NULL) callSbrk();
 
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
 *  Allocates n words in region rAddr. It will make sure, that there    *
 *  is space for the n words before doing the allocation.               *
 *  Pre-condition: n <= ALLOCATABLE_WORDS_IN_REGION_PAGE                *
 *----------------------------------------------------------------------*/
int *alloc (int rAddr, int n) { 
  int *t1;
  int *t2;
  int *t3;
  Ro *rp;

#ifdef PROFILING
  int *i;
  int j;
#endif

  rp = (Ro *) clearStatusBits(rAddr);

#ifdef PROFILING
  allocNowInf += n-sizeObjectDesc; /* When profiling we also allocate an object descriptor. */
  maxAlloc = max(maxAlloc, allocNowInf+allocNowFin);
  rp->allocNow += n-sizeObjectDesc;
  profTabIncrAllocNow(rp->regionId, n-sizeObjectDesc);

  callsOfAlloc++;
  maxAllocInf = max(allocNowInf, maxAllocInf);
  allocProfNowInf += sizeObjectDesc;
  if (maxAllocInf == allocNowInf) maxAllocProfInf = allocProfNowInf;
  rp->allocProfNow += sizeObjectDesc;
#endif

  t1 = rp->a;
  t2 = t1 + n;

  t3 = rp->b;
  if (t2 > t3) {
#ifdef PROFILING
    /* insert zeros in the rest of the current region page */
    for ( i = t1 ; i < t3 ; i++ )  *i = notPP;
#endif PROFILING
    alloc_new_block(rp);

    t1 = rp->a;
    t2 = t1+n;
  }
  rp->a = t2;
  /*printf("now returning from alloc(%d)\n",n);*/
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
  extern Klump * freelist;
  
#ifdef PROFILING
  int *i;
  Klump *temp;
  int j;
#endif

  rp = (Ro *) clearStatusBits(rAdr);



#ifdef PROFILING
  callsOfResetRegion++;
  j = NoOfPagesInRegion(rp);

  /* There is always at-least one page in a region. */
  noOfPages -= j-1;
  profTabDecrNoOfPages(rp->regionId, j-1);

  allocNowInf -= rp->allocNow;
  profTabDecrAllocNow(rp->regionId, rp->allocNow);
  allocProfNowInf -= rp->allocProfNow;
#endif

  /* There is always at least one page in a region. */
  if ( (rp->fp)->k.n != NULL ) { /* There are more than one page in the region. */
    (((Klump *)rp->b)-1)->k.n = freelist;
    freelist = (rp->fp)->k.n;
    (rp->fp)->k.n = NULL;
  }

  rp->a = (int *)(&(rp->fp)->k.i); /* beginning of klump in first page */
  rp->b = (int *)((rp->fp)+1);     /* end of klump in first page */

#ifdef PROFILING
  rp->allocNow = 0;
  rp->allocProfNow = 0;
#endif

  return rAdr; /* We preserve rAdr and the status bits. */
}

/*-------------------------------------------------------------------------*
 *deallocateRegionsUntil:                                                  *
 *  It is called with rAddr=sp, which do not nessesaraly point at a region *
 *  description. It deallocates all regions that are placed over sp.       *
 *  The function does not return or alter anything.                        *
 *-------------------------------------------------------------------------*/
void deallocateRegionsUntil(int rAddr) { 
  Ro *rp;
  extern Ro * topRegion;

  rp = (Ro *) clearStatusBits(rAddr);
  
#ifdef PROFILING
  callsOfDeallocateRegionsUntil++;
  while ((FiniteRegionDesc *)rp <= topFiniteRegion)
    deallocRegionFiniteProfiling();
#endif

  while (rp <= topRegion) 
    {/*printf("rp: %0x, topRegion %0x\n",rp,topRegion);*/
    deallocateRegion();}

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
 * allocRegionInfiniteProfiling(roAddr, regionId)                          *
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
int *allocRegionInfiniteProfiling(Ro *roAddr, unsigned int regionId) { 
  Klump *kp;
  extern Klump * freelist;

  callsOfAllocateRegionInf++;
  maxNoOfPages = max(++noOfPages, maxNoOfPages);
  profTabMaybeIncrMaxNoOfPages(regionId);
  regionDescUseInf += (sizeRo-sizeRoProf);
  maxRegionDescUseInf = max(maxRegionDescUseInf,regionDescUseInf);
  regionDescUseProfInf += sizeRoProf;
  maxRegionDescUseProfInf = max(maxRegionDescUseProfInf,regionDescUseProfInf);

  if (freelist==NULL) callSbrk();

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

  /* We have to set the infinitebit. */
  roAddr = (Ro *) setInfiniteBit((int) roAddr);

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
void allocRegionFiniteProfiling(FiniteRegionDesc *rdAddr, unsigned int regionId, int size) { 
  ObjectDesc *objPtr;  
  ProfTabList* p;
  int index;

  allocNowFin += size;                                  /* necessary for graph drawing */
  maxAlloc = max(maxAlloc, allocNowFin+allocNowInf);    /* necessary for graph drawing */

  if (showStat) {
    callsOfAllocateRegionFin++;
    maxAllocFin = max(allocNowFin, maxAllocFin);
    allocProfNowFin += sizeObjectDesc;
    regionDescUseProfFin += sizeFiniteRegionDesc;
    if (allocNowFin == maxAllocFin) {
      maxAllocProfFin = allocProfNowFin;
      maxRegionDescUseProfFin = regionDescUseProfFin;
    }
  }

  /* Inlining of function profTabIncrAllocNow(regionId, size);
   * this update is necessary for graph drawing */
  index = profHashTabIndex(regionId);                             
  for (p=profHashTab[index]; p != NULL; p=p->next)
    if (p->regionId == regionId) goto escape;
  p = profTabListInsertAndInitialize(profHashTab[index], regionId);
  profHashTab[index] = p;
escape:
  p->allocNow += size;
  if (p->allocNow > p->maxAlloc) p->maxAlloc = p->allocNow;
  /* inlining end */

  rdAddr->p = topFiniteRegion; /* link to previous region description on stack */
  rdAddr->regionId = regionId; /* Put name on region in descriptor. */
  topFiniteRegion = (FiniteRegionDesc *)(rdAddr); /* pointer to topmost region description on stack */

  objPtr = (ObjectDesc *)(rdAddr + 1); /* We also put the object descriptor onto the stack. */
  objPtr->atId = notPrgPoint;
  objPtr->size = size;

  return;
}

/*-----------------------------------------------------------------*
 * deallocRegionFiniteProfiling:                                   *
 * topFiniteRegion has to point at the bottom address of the       *
 * finite region descriptor, which will be the new stack address.  *
 *-----------------------------------------------------------------*/
int *deallocRegionFiniteProfiling(void) { 
  int *sp;
  int size;
  
  size = ((ObjectDesc *) (topFiniteRegion + 1))->size;
  allocNowFin -= size;                                    /* necessary for graph drawing */

  if (showStat) {
    callsOfDeallocateRegionFin++;
    profTabDecrAllocNow(topFiniteRegion->regionId, size);
    allocProfNowFin -= sizeObjectDesc;
    regionDescUseProfFin -= sizeFiniteRegionDesc;
  }

  sp = (int *) topFiniteRegion;
  topFiniteRegion = topFiniteRegion->p;                   /* pop ptr. to prev. region desc. */
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

  /*
  if (n + sizeObjectDesc> ALLOCATABLE_WORDS_IN_REGION_PAGE) {
    sprintf(errorStr, "ERROR -- allocProfiling is called with request for object of size %d words(not including the size of the object descriptor) and ALLOCATABLE_WORDS_IN_REGION_PAGE is only %d\nTo solve the problem,\nincrease the value of ALLOCATABLE_WORDS_IN_REGION_PAGE (in Region.h) to\nthe number of words you need and then recompile\nthe runtime system.\n",
	    n, ALLOCATABLE_WORDS_IN_REGION_PAGE);
    printERROR(errorStr);
  } */

  /* allocate object descriptor and object */
  res = alloc(rAddr, n+sizeObjectDesc);
  /* initialize object descriptor */
  ((ObjectDesc *)res)->atId = pPoint;
  ((ObjectDesc *)res)->size = n;
  /* return pointer to user data */
  res = (int *)(((ObjectDesc *)res) + 1);

  return res;
}


/*------------------------------------------------------------------*
 * storePrgPointProfiling:                                          *
 * objPtr is pointing at the object, and not the object descriptor. *
 * We could make an test on the size field each time pp i set.      *
 *------------------------------------------------------------------*/
void storePrgPointProfiling(int pPoint, int *objPtr) {

  if ( (((ObjectDesc *)objPtr)-1)->size == dummyDouble ) {
    /* We have an unaligned double in a finite region.      */
    /* The object descriptor is therefore one address lower */
    /* than usual.                                          */
   
    (((ObjectDesc *) (((int *)objPtr)-1))-1)->atId = pPoint;
  }
  else {
    (((ObjectDesc *)objPtr)-1)->atId = pPoint;
  }

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
   
    frAddr = ((FiniteRegionDesc *) (((ObjectDesc *) (((int *)doublePtr)-1))-1))-1;
  }
  else
    frAddr = ((FiniteRegionDesc *) (((ObjectDesc *)doublePtr)-1))-1;
  
  regionId = frAddr->regionId;

  allocNowFin += size; 
  maxAllocFin = max(allocNowFin, maxAllocFin);
  maxAlloc = max(maxAlloc, allocNowFin+allocNowInf);
  profTabIncrAllocNow(regionId, size);

  return doublePtr;

}

#endif /*PROFILING*/





