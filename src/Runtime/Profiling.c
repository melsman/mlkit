/*----------------------------------------------------------------*
 *                        Profiling                               *
 *----------------------------------------------------------------*/

/* Only include this file if PROFILING is defined...  The queueMark
 * function should be defined if PROFILING is not defined, however;
 * look at the bottom. */

#include <stdlib.h>
#include <stdio.h>
#include <signal.h>      // used by signal
#include <time.h>        // used by clock
#include <sys/time.h>    // used by setitimer

#include "Profiling.h"
#include "Region.h"
#include "Tagging.h"
#include "String.h"
#include "Exception.h"

#ifdef PROFILING

/*----------------------------------------------------------------*
 * Global declarations                                            *
 *----------------------------------------------------------------*/
long *stackBot;
long timeToProfile;
long maxStack;         // max. stack size from check_stack
long *maxStackP=NULL;  // Max. stack addr. from ProfileTick
long tempCount;
long tellTime;         /* 1, if the next profile tick should print out the
		       * current time - 0 otherwise */

struct itimerval rttimer;
struct itimerval old_rttimer;
int    profileON = TRUE; /* if false profiling is not started after a profileTick. */

char * freeProfiling;  /* Pointer to free-chunk of mem. to profiling data. */
long freeProfilingRest; /* Number of bytes left in freeProfiling-chunk.     */

TickList * firstTick; /* Pointer to data for the first tick. */
TickList * lastTick;  /* Pointer to data for the last tick. */

/* The following two global arrays are used as hash tables during
 * a profile tick. */
RegionListHashList * regionListHashTable[REGION_LIST_HASH_TABLE_SIZE];  /* Used as hash table into a region list. */
ObjectListHashList * objectListHashTable[OBJECT_LIST_HASH_TABLE_SIZE];  /* Used as hash table into an object list. */

ProfTabList * profHashTab[PROF_HASH_TABLE_SIZE];  /* Hash table for information collected during execution */

long profTabCountDebug = 0;

unsigned long numberOfTics=0; /* Number of profilings so far. */
unsigned long lastCpuTime=0;  /* CPU time after last tick. */
unsigned long cpuTimeAcc=0;   /* Used time by program excl. profiling. */

long noTimer =                                  /* Profile with a constant number of function calls. */
    ITIMER_REAL+ITIMER_VIRTUAL+ITIMER_PROF;    /* A number different from the other constants.      */
long profType = ITIMER_VIRTUAL; /* Type of profiling to use */
long signalType = SIGVTALRM;    /* Signal to catch depending on profType. */
long profNo = 10000;
long microsec = 0;
long sec = 1;
long verboseProfileTick = 0;
long printProfileTab = 0;
long exportProfileDatafile = 1;
long showStat = 0;

char  logName[100]="profile.rp";   /* Name of log file to use. */
FILE* logFile;
int noOfTickInFile = 0;
char prgName[100];

long doing_prof = 0;
long raised_exn_interupt_prof = 0;
long raised_exn_overflow_prof = 0;

static unsigned long min(unsigned long a, unsigned long b) {
  return (a<b)?a:b;
}

/*--------------------------------------
 * Hash table to hold profiling table
 * mapping region ids to objects of type
 * profTabList (see Profiling.h)
 *--------------------------------------*/

void initializeProfTab(void) {
  long i;
  /*  printf("Initializing profTab\n"); */
  for (i = 0 ; i < PROF_HASH_TABLE_SIZE ; i++)
    profHashTab[i]=NULL;
  return;
}

/*
long profSize(ProfTabList* p) {
  long count = 0;
  for ( ; p != NULL; p=p->next) {
    count++ ;
  }
  return count;
}
*/

long profTabSize(void) {
  long count, i;
  ProfTabList* p;
  for (count = 0, i = 0 ; i < PROF_HASH_TABLE_SIZE ; i++)
    for (p=profHashTab[i]; p != NULL; p=p->next, count++) {}
  return count;
}

ProfTabList* profTabListInsertAndInitialize(ProfTabList* p, long regionId) {
  ProfTabList* pNew;
  /*  checkProfTab("profTabListInsertAndInitialize.enter"); */

  profTabCountDebug ++;
  /*
      printf("Entering profTabListInsertAndInitialize; regionId = %d, profSize = %d, count = %d\n",
	 regionId, profSize(p), profTabCountDebug);
  */
  pNew = (ProfTabList*)allocMemProfiling_xx(sizeof(ProfTabList));
  if (pNew == (ProfTabList*) -1) {
    perror("profTabListInsertAndInitialize error\n");
    exit(-1);
  }
  pNew->regionId=regionId;
  pNew->noOfPages=0;
  pNew->maxNoOfPages=0;
  pNew->allocNow=0;
  pNew->maxAlloc=0;
  pNew->next=p;
  /*  checkProfTab("profTabListInsertAndInitialize.exit"); */
  return pNew;
}

void profTabMaybeIncrMaxNoOfPages(long regionId) {
  ProfTabList* p;
  long index;

  /*  checkProfTab("profTabMaybeIncrMaxNoOfPages.enter"); */

  index = profHashTabIndex(regionId);

  for (p=profHashTab[index]; p != NULL; p=p->next)
    if (p->regionId == regionId) {
      if (p->noOfPages >= p->maxNoOfPages) p->maxNoOfPages = p->noOfPages;
      /* checkProfTab("profTabMaybeIncrMaxNoOfPages.exit1"); */
      return;
    };
  p = profTabListInsertAndInitialize(profHashTab[index], regionId);
  profHashTab[index] = p;
  p->maxNoOfPages = 1;
  p->noOfPages = 1;
  /* checkProfTab("profTabMaybeIncrMaxNoOfPages.exit2"); */
  return;
}

long profTabGetNoOfPages(long regionId) {
  ProfTabList* p;
  /* checkProfTab("profTabGetNoOfPages.enter"); */
  for (p=profHashTab[profHashTabIndex(regionId)]; p != NULL; p=p->next)
    if (p->regionId == regionId) {
      /* checkProfTab("profTabGetNoOfPages.exit1"); */
      return p->noOfPages;
    }
  /* checkProfTab("profTabGetNoOfPages.exit2"); */
  return 0;
}

void profTabIncrNoOfPages(long regionId, long i) {
  ProfTabList* p;
  long index;
  /* checkProfTab("profTabIncrNoOfPages.enter"); */

  index = profHashTabIndex(regionId);
  for (p=profHashTab[index]; p != NULL; p=p->next)
    if (p->regionId == regionId) {
      p->noOfPages = p->noOfPages + i;
      /* checkProfTab("profTabIncrNoOfPages.exit1"); */
      return;
    };
  p = profTabListInsertAndInitialize(profHashTab[index], regionId);
  profHashTab[index] = p;
  p->maxNoOfPages = 1;
  p->noOfPages = 1;
  /* checkProfTab("profTabIncrNoOfPages.exit2"); */
  return;
}

void profTabDecrNoOfPages(long regionId, long i) {
  ProfTabList* p;
  /* checkProfTab("profTabDecrNoOfPages.enter"); */
  for (p=profHashTab[profHashTabIndex(regionId)]; p != NULL; p=p->next)
    if (p->regionId == regionId) {
      p->noOfPages = p->noOfPages - i;
      /* checkProfTab("profTabDecrNoOfPages.exit"); */
      return;
    };
  printf("regionId is %ld\n", regionId);
  perror("profTabDecrNoOfPages error");
  exit(-1);
}

void profTabDecrAllocNow(long regionId, long i, char *s) {
  ProfTabList* p;
  /* checkProfTab("profTabDecrAllocNow.enter"); */
  for (p=profHashTab[profHashTabIndex(regionId)]; p != NULL; p=p->next)
    if (p->regionId == regionId) {
      p->allocNow = p->allocNow - i;
      /* checkProfTab("profTabDecrAllocNow.exit"); */
      return;
    };
  fprintf(stderr, "Error.%s: regionId %ld does not exist in profiling table\n", s, regionId);
  fprintf(stderr, "profTabCountDebug = %ld, profTabSize = %ld\n", profTabCountDebug,
	  profTabSize());
  printProfTab();
  perror("profTabDecrAllocNow error\n");
  exit(-1);
}

void profTabIncrAllocNow(long regionId, long i) {
  ProfTabList* p;
  long index;
  /* checkProfTab("profTabIncrAllocNow.enter"); */

  index = profHashTabIndex(regionId);
  for (p=profHashTab[index]; p != NULL; p=p->next) {
    if (p->regionId == regionId) {
      p->allocNow += i;
      if (p->allocNow > p->maxAlloc) p->maxAlloc = p->allocNow;
      /* checkProfTab("profTabIncrAllocNow.exit1"); */
      return;
    }
  }
  p = profTabListInsertAndInitialize(profHashTab[index], regionId);
  profHashTab[index] = p;
  p->allocNow += i;
  if (p->allocNow > p->maxAlloc) p->maxAlloc = p->allocNow;
  /* checkProfTab("profTabIncrAllocNow.exit2"); */
  return;
}


/* ---------------------------------------------------------- *
 * Hash table to hold LOCAL region map. Hash table used
 * locally during a profile tick to make lookup fast.
 * ---------------------------------------------------------- */

void initializeRegionListTable(void) {
  long i;
  for (i = 0 ; i < REGION_LIST_HASH_TABLE_SIZE; i++ )
    regionListHashTable[i] = NULL;
  return;
}

void insertRegionListTable(long regionId, RegionList* rl) {
  RegionListHashList* p;
  long index;
  index = regionListHashTabIndex(regionId);
  for (p=regionListHashTable[index]; p != NULL; p=p->next)
    if (p->regionId == regionId) {
      p->rl = rl;
      return;
    };
  p = (RegionListHashList*)allocMemProfiling_xx(sizeof(RegionListHashList));     /* create element */
  if (p == (RegionListHashList*) -1) {
    perror("insertRegionListTable error\n");
    exit(-1);
  }
  p->regionId = regionId;
  p->rl = rl;
  p->next = regionListHashTable[index];
  regionListHashTable[index] = p;         /* update hash table; new element is stored in front */
  return;
}

RegionList* lookupRegionListTable(long regionId) {
  RegionListHashList* p;
  long index;
  index = regionListHashTabIndex(regionId);
  for (p=regionListHashTable[index]; p != NULL; p=p->next)
    if (p->regionId == regionId) return p->rl;
  return NULL;
}

/* ---------------------------------------------------------- *
 * Hash table to hold LOCAL object map. The hash table is used
 * locally during a profile tick to make lookup fast.
 * ---------------------------------------------------------- */

void initializeObjectListTable(void) {
  long i;
  for (i = 0; i < OBJECT_LIST_HASH_TABLE_SIZE; i++)
    objectListHashTable[i] = NULL;
  return;
}

void insertObjectListTable(long atId, ObjectList* ol) {
  ObjectListHashList* p;
  long index;
  index = objectListHashTabIndex(atId);
  for (p=objectListHashTable[index]; p != NULL; p=p->next)
    if (p->atId == atId) {
      p->ol = ol;
      return;
    };
  p = (ObjectListHashList*)allocMemProfiling_xx(sizeof(ObjectListHashList));   /* create element */
  if (p == (ObjectListHashList*) -1) {
    perror("insertObjectListTable error\n");
    exit(-1);
  }
  p->atId = atId;
  p->ol = ol;
  p->next = objectListHashTable[index];
  objectListHashTable[index] = p;         /* update hash table; new element is stored in front */
  return;
}

ObjectList* lookupObjectListTable(long atId) {
  ObjectListHashList* p;
  long index;
  index = objectListHashTabIndex(atId);
  for (p=objectListHashTable[index]; p != NULL; p=p->next)
    if (p->atId == atId) return p->ol;
  return NULL;
}

/*----------------------------------------------------------------------*
 *                        Statistical operations.                       *
 *----------------------------------------------------------------------*/

/* This function sets the flags 'tellTime' so that next time
   a tick is made, the time is printed on stdout */

void
queueMarkProf(StringDesc *str, long pPoint)
{
    tellTime = 1;
    fprintf(stderr,"Reached \"%s\"\n", &(str->data));
    return;
}

/* Calculate the allocated and used space in a region. */
/* All instantiated regions with this region name is       */
/* calculated as one region.                               */
void
AllocatedSpaceInARegion(Ro *rp)
{
  long n;

  n = profTabGetNoOfPages(rp->regionId) * ALLOCATABLE_WORDS_IN_REGION_PAGE * sizeof(long*);
  fprintf(stderr,"    Allocated bytes in region %5zd: %5ld\n",rp->regionId, n);
  return;
}

/* Prints all pages in the generation */
void
PrintGen(Gen *gen)
{
  long i;
  Rp *p;
  if ( gen )
    {
      fprintf(stderr,"\n  Address of Gen: %p, First free word: %p, Border of region: %p\n     ",gen,gen->a,gen->b);
      for ( p = clear_fp(gen->fp) , i = 1 ; p ; p = p->n , i++ )
	{
	  fprintf(stderr,"-->Page%2ld:%p",i,p);
	  if (i%3 == 0)
	    fprintf(stderr,"\n     ");
	}
      fprintf(stderr,"\n");
    }
}

/* Prints all pages in the region. */
void
PrintRegion(Region r)
{
  fprintf(stderr,"\nAddress of Ro: %p\n     ",r);

  PrintGen(&(r->g0));
#ifdef ENABLE_GEN_GC
  PrintGen(&(r->g1));
#endif /* ENABLE_GEN_GC */
}

void
resetProfiler()
{
  outputProfilePre();
  initializeProfTab();
  lastCpuTime = (unsigned int)clock();
  if (profType == noTimer)
    {
      timeToProfile = 1;
    }
  else
    {
      timeToProfile = 0;
      rttimer.it_value.tv_sec = sec;         /* Time in seconds to first tick. */
      rttimer.it_value.tv_usec = microsec;   /* Time in microseconds to first tick. */
      rttimer.it_interval.tv_sec = 0;        /* Time in seconds between succeding ticks. */
      rttimer.it_interval.tv_usec = 0;       /* Time in microseconds between succeding ticks. */

      signal(signalType, AlarmHandler);

      profiling_on();
    }

  if (verboseProfileTick)
    {
      fprintf(stderr,  "---------------------Profiling-Enabled---------------------\n");
      if (profType == noTimer)
	{
	  fprintf(stderr," The profile timer is turned off; a profile tick occurs\n");
	  fprintf(stderr,"every %ldth entrance to a function.\n", profNo);
	}
      if (profType == ITIMER_REAL)
	fprintf(stderr," The profile timer (unix real timer) is turned on.\n");
      if (profType == ITIMER_VIRTUAL)
	fprintf(stderr," The profile timer (unix virtual timer) is turned on.\n");
      if (profType == ITIMER_PROF)
	fprintf(stderr," The profile timer (unix profile timer) is turned on.\n");
      if (microsec != 0 && profType != noTimer)
	fprintf(stderr," A profile tick occurs every %ldth microsecond.\n", microsec);
      if (sec != 0 && profType != noTimer)
	fprintf(stderr," A profile tick occurs every %ldth second.\n", sec);
      if (exportProfileDatafile)
	fprintf(stderr," Profiling data is exported to file %s.\n", logName);
      else
	fprintf(stderr," No profiling data is exported.\n");
      fprintf(stderr,  "-----------------------------------------------------------\n");
    }
  return;
}

void
checkProfTab(char* s)
{
  unsigned long i;
  ProfTabList* p;
  for ( i = 0 ; i < PROF_HASH_TABLE_SIZE ; i++ )
    for ( p = profHashTab[i] ; p ; p = p->next )
      if ( p->regionId > 1000000 )
	{
	  printProfTab();
	  printf("Mysterious regionId (%ld) in ProfTab: %s\n", p->regionId, s);
	  exit(-1);
	}
}

void
printProfTab()
{
  long i;
  long noOfPagesTab, maxNoOfPagesTab;
  long allocNowTab, maxAllocTab;
  long noOfPagesTot, maxNoOfPagesTot;
  long allocNowTot, maxAllocTot;
  ProfTabList* p;

  noOfPagesTot = 0;
  maxNoOfPagesTot = 0;
  allocNowTot = 0;
  maxAllocTot = 0;

  fprintf(stderr,"\n\nPRINTING PROFILING TABLE.\n");
  for ( i = 0 ; i < PROF_HASH_TABLE_SIZE ; i++ )
    for (p=profHashTab[i];p!=NULL;p=p->next) {
      noOfPagesTab = p->noOfPages;
      noOfPagesTot += noOfPagesTab;
      maxNoOfPagesTab = p->maxNoOfPages;
      maxNoOfPagesTot += maxNoOfPagesTab;
      allocNowTab = p->allocNow;
      allocNowTot += allocNowTab;
      maxAllocTab = p->maxAlloc;
      maxAllocTot += maxAllocTab;
      /*      if (maxNoOfPagesTab)  */
	fprintf(stderr,"    profTab[rId%5ld]: noOfPages = %8ld, maxNoOfPages = %8ld, allocNow = %8ld, maxAlloc = %8ld\n",
		p->regionId, noOfPagesTab, maxNoOfPagesTab, allocNowTab*sizeof(long*), maxAllocTab*sizeof(long*));
    }
  fprintf(stderr,      "    ---------------------------------------------------------------------------------------------------\n");
  fprintf(stderr,      "                          %8ld     SUM OF MAX: %8ld         Bytes: %8ld      Bytes: %8ld\n",
	  noOfPagesTot, maxNoOfPagesTot, allocNowTot*sizeof(long*), maxAllocTot*sizeof(long*));
  fprintf(stderr,      "    ===================================================================================================\n");

}

void
Statistics()
{
  double Mb = 1024.0*1024.0;

  if (showStat) {
    fprintf(stderr,"\n*************Region statistics***************\n");

    if (printProfileTab) printProfTab();

    /*    fprintf(stderr,"  Size of finite region descriptor: %d bytes\n",sizeof(FiniteRegionDesc)); */
    fprintf(stderr,"\nMALLOC\n");
    fprintf(stderr,"  Number of calls to malloc for regions: %ld\n",callsOfSbrk);
    fprintf(stderr,"  Alloc. in each malloc call: %ld bytes\n", BYTES_ALLOC_BY_SBRK);
    fprintf(stderr,"  Total allocation by malloc: %ld bytes (%.1fMb)\n", BYTES_ALLOC_BY_SBRK*callsOfSbrk,
	    (BYTES_ALLOC_BY_SBRK*callsOfSbrk)/Mb );

    fprintf(stderr,"\nREGION PAGES\n");
    fprintf(stderr,"  Size of one page: %ld bytes\n",ALLOCATABLE_WORDS_IN_REGION_PAGE*sizeof(long*));
    fprintf(stderr,"  Max number of allocated pages: %ld\n",maxNoOfPages);
    fprintf(stderr,"  Number of allocated pages now: %ld\n",noOfPages);
    fprintf(stderr,"  Max space for region pages: %ld bytes (%.1fMb)\n",
	    maxNoOfPages*ALLOCATABLE_WORDS_IN_REGION_PAGE*(sizeof(long*)), (maxNoOfPages*ALLOCATABLE_WORDS_IN_REGION_PAGE*(sizeof(long*)))/Mb);

    fprintf(stderr,"\nINFINITE REGIONS\n");
    /*    fprintf(stderr,"  Size of infinite reg. desc. (incl. prof info): %d bytes\n",sizeRo*sizeof(long*)); */
    fprintf(stderr,"  Size of infinite region descriptor: %ld bytes\n",(sizeRo-sizeRoProf)*(sizeof(long*)));
    fprintf(stderr,"  Number of calls to allocateRegionInf: %ld\n",callsOfAllocateRegionInf);
    fprintf(stderr,"  Number of calls to deallocateRegionInf: %ld\n",callsOfDeallocateRegionInf);
    fprintf(stderr,"  Number of calls to alloc: %ld\n",callsOfAlloc);
    fprintf(stderr,"  Number of calls to resetRegion: %ld\n",callsOfResetRegion);
    fprintf(stderr,"  Number of calls to deallocateRegionsUntil: %ld\n",callsOfDeallocateRegionsUntil);

    fprintf(stderr,"\nALLOCATION\n");
    /*
    fprintf(stderr,"  Alloc. space in infinite regions: %d bytes (%.1fMb)\n", allocNowInf*sizeof(long*), (allocNowInf*sizeof(long*))/Mb);
    fprintf(stderr,"  Alloc. space in finite regions: %d bytes (%.1fMb)\n", allocNowFin*sizeof(long*), (allocNowFin*sizeof(long*))/Mb);
    fprintf(stderr,"  Alloc. space in regions: %d bytes (%.1fMb)\n", (allocNowInf+allocNowFin)*sizeof(long*),((allocNowInf+allocNowFin)*sizeof(long*))/Mb);
    */
    fprintf(stderr,"  Max alloc. space in pages: %ld bytes (%.1fMb)\n", maxAllocInf*(sizeof(long*)),(maxAllocInf*(sizeof(long*)))/Mb);
    /*
    fprintf(stderr,  "      Space in regions at that time used on profiling: %d bytes (%4.1fMb)\n", maxAllocProfInf*sizeof(long*),
	    (maxAllocProfInf*sizeof(long*))/Mb);
    fprintf(stderr,"  -------------------------------------------------------------------------------\n");
    */
    fprintf(stderr,"    incl. prof. info: %ld bytes (%.1fMb)\n",
	    (maxAllocProfInf+maxAllocInf)*(sizeof(long*)), ((maxAllocProfInf+maxAllocInf)*(sizeof(long*)))/Mb);
    fprintf(stderr,"  Infinite regions utilisation (%ld/%ld): %2.0f%%\n",
	    (maxAllocProfInf+maxAllocInf)*(sizeof(long*)),
	    (maxNoOfPages*ALLOCATABLE_WORDS_IN_REGION_PAGE*(sizeof(long*))),
	    ((maxAllocProfInf+maxAllocInf)*1.0*(sizeof(long*)))/(maxNoOfPages*ALLOCATABLE_WORDS_IN_REGION_PAGE*1.0*(sizeof(long*)))*100.0);
    fprintf(stderr,"  Number of allocated large objects: %ld\n", allocatedLobjs);
    fprintf(stderr,"\nSTACK\n");
    fprintf(stderr,"  Number of calls to allocateRegionFin: %ld\n",callsOfAllocateRegionFin);
    fprintf(stderr,"  Number of calls to deallocateRegionFin: %ld\n",callsOfDeallocateRegionFin);
    fprintf(stderr,"  Max space for finite regions: %ld bytes (%.1fMb)\n", maxAllocFin*(sizeof(long*)),
	    (maxAllocFin*(sizeof(long*)))/Mb);
    fprintf(stderr,"  Max space for region descs: %ld bytes (%.1fMb)\n",
	    maxRegionDescUseInf*sizeof(long*), (maxRegionDescUseInf*(sizeof(long*)))/Mb);
    fprintf(stderr,"  Max size of stack: %ld bytes (%.1fMb)\n",
	   ((long)stackBot)-((long)maxStack)-(maxProfStack*(sizeof(long*))), (((long)stackBot)-((long)maxStack)-(maxProfStack*(sizeof(long*))))/Mb);
    fprintf(stderr,"    incl. prof. info: %ld bytes (%.1fMb)\n",
	    ((long)stackBot)-((long)maxStack), ((((long)stackBot)-((long)maxStack))*1.0)/Mb);
    fprintf(stderr,"    in profile tick: %ld bytes (%.1fMb)\n",
	    ((long)stackBot)-((long)maxStackP), (((long)stackBot)-((long)maxStackP))/Mb);
    fprintf(stderr,"Number of profile ticks: %d\n", noOfTickInFile);
    /*
    fprintf(stderr,  "      Space used on prof. info. at that time: %d bytes (%.1fMb)\n",
	    maxRegionDescUseProfInf*sizeof(long*), (maxRegionDescUseProfInf*sizeof(long*))/Mb);
    fprintf(stderr,"  ---------------------------------------------------------------------------------------------\n");
    fprintf(stderr,"  Max space used on infinite region descs on stack: %d bytes (%4.1fMb)\n",
	    (maxRegionDescUseInf+maxRegionDescUseProfInf)*sizeof(long*),((maxRegionDescUseInf+maxRegionDescUseProfInf)*sizeof(long*))/Mb);
    fprintf(stderr,"      Space used on profiling information at that time: %d bytes (%4.1fMb)\n",
	    (maxAllocProfFin+maxRegionDescUseProfFin)*sizeof(long*), ((maxAllocProfFin+maxRegionDescUseProfFin)*sizeof(long*))/Mb);
    fprintf(stderr,"  -------------------------------------------------------------------------------------------\n");
    fprintf(stderr,"  Max space used on finite regions on stack: %d bytes (%4.1fMb)\n",
	    (maxAllocFin+maxAllocProfFin+maxRegionDescUseProfFin)*sizeof(long*),((maxAllocFin+maxAllocProfFin+maxRegionDescUseProfFin)*sizeof(long*))/Mb);
    fprintf(stderr,"    Space used on profiling information at that time: %d bytes (%4.1fMb)\n",
	    maxProfStack*sizeof(long*), (maxProfStack*sizeof(long*))/Mb);
    fprintf(stderr,"  -------------------------------------------------------------------------------\n");
	    */
    fprintf(stderr,"\n*********End of region statistics*********\n");
  }
  return;
}


/***************************************************************************
 *                Functions for the profiling tool.                        *
 * This module contains functions used when profiling.                     *
 *                                                                         *
 * profileTick(stackTop)                                                   *
 * printProfile()                                                          *
 * outputProfile()                                                         *
 * AlarmHandler()                                                          *
 * callSbrkProfiling() Used to allocate memory to profiling data.          *
 * allocMemProfiling(n) Allocate n bytes to profiling data.                *
 ***************************************************************************/

/*------------------------------------------------------*
 * If an error occurs while profiling, then print the   *
 * error and stop.                                      *
 *------------------------------------------------------*/
char errorStr[255];
void
profileERROR(char *errorStr)
{
  fprintf(stderr,"\n***********************ERROR*****************************\n");
  fprintf(stderr,"%s\n", errorStr);
  fprintf(stderr,"\n***********************ERROR*****************************\n");
  exit(-1);
}

/*-----------------------------------------------*
 * This function prints the contents of a finite *
 * region on screen.                             *
 *-----------------------------------------------*/
void
pp_finite_region (FiniteRegionDesc *frd)
{
  ObjectDesc *obj;
  obj = (ObjectDesc *) (frd+1);
  fprintf(stderr,"FRDid: %zu, next: %zu, objectId: %lu, objSize: %zu\n",
	 frd->regionId, (size_t) frd, obj->atId, obj->size);
  return;
}

/*--------------------------------------------------*
 * This function prints the contents of an infinite *
 * region on screen.                                *
 *--------------------------------------------------*/
void
pp_infinite_region_gen (Gen *gen)
{
  ObjectDesc *fObj;
  Rp *rp;

  fprintf(stderr,"Generation %lu at address %p\n", generation(*gen),gen);
  for( rp = clear_fp(gen->fp) ; rp ; rp = rp->n )
    {
      fObj = (ObjectDesc *) (((long *)rp)+HEADER_WORDS_IN_REGION_PAGE);
      while ( ((long *)fObj < ((long *)rp)+ALLOCATABLE_WORDS_IN_REGION_PAGE+HEADER_WORDS_IN_REGION_PAGE)
	      && (fObj->atId!=notPP) )
	{
	  fprintf(stderr,"ObjAtId %zu, Size: %zu\n", fObj->atId, fObj->size);
	  fObj=(ObjectDesc *)(((size_t *)fObj)+((fObj->size)+sizeObjectDesc)); /* Find next object. */
	}
    }
  return;
}

void
pp_infinite_region (Region r)
{
  r = clearStatusBits(r);

  fprintf(stderr,"Region %zu\n", r->regionId);
  pp_infinite_region_gen(&(r->g0));
#ifdef ENABLE_GEN_GC
  pp_infinite_region_gen(&(r->g1));
#endif /* ENABLE_GEN_GC */

  return;
}

/*---------------------------------------------------*
 * This function prints the contents of all infinite *
 * regions on screen.                                *
 *---------------------------------------------------*/
void
pp_infinite_regions()
{
  Region r;

  for ( r = TOP_REGION ; r ; r = r->p )
    pp_infinite_region(r);

  return;
}


/*------------------------------------------------------*
 * profiling_on                                         *
 *   Sets alarm for profiling.                          *
 *------------------------------------------------------*/
void
profiling_on()
{
  setitimer(profType, &rttimer, &old_rttimer);
  profileON = TRUE;
  if (verboseProfileTick)
    fprintf(stderr,"Profiling turned on...\n");
  return;
}

/*------------------------------------------------------*
 * profiling_off                                        *
 *   Stop alarm for profiling.                          *
 *------------------------------------------------------*/
void
profiling_off()
{
  struct itimerval zerotimer;

  zerotimer.it_value.tv_sec = 0;        /* Time in seconds to first tick. */
  zerotimer.it_value.tv_usec = 0;       /* Time in microseconds to first tick. */
  zerotimer.it_interval.tv_sec = 0;     /* Time in seconds between succeding ticks. */
  zerotimer.it_interval.tv_usec = 0;    /* Time in microseconds between succeding ticks. */
  setitimer(profType, &zerotimer, &old_rttimer);
  profileON = FALSE;
  if (verboseProfileTick)
    fprintf(stderr,"Profiling turned off...\n");
  return;
}

/*------------------------------------------------------*
 * allocMemProfiling                                    *
 *   Takes i bytes from the free-chunk of memory        *
 *   allocated for profiling data.                      *
 *------------------------------------------------------*/
char *
allocMemProfiling_xx(long i)
{
  char * p;
  char * tempPtr;

  tempPtr = (char *)malloc(i);
  if ( tempPtr == NULL )
    {
      perror("malloc error in allocMemProfiling\n");
      exit(-1);
    }

  if ( ((long)tempPtr) % (sizeof(long*)) )
    {
      perror("allocMemProfiling_xx not aligned\n");
      exit(-1);
    }

  // for debugging: initialize elements
  for ( p = tempPtr ; p < tempPtr+i ; p++ )
    {
      *p = 1;  /*dummy*/
    }

  return tempPtr;
}

void
freeTick(TickList *tick)
{
  ObjectList *o, *n_o;
  RegionList *r, *n_r;

  debug(printf("[freeTick..."));
  r = tick->fRegion;
  while( r )
    {
      n_r = r->nRegion;
      o = r->fObj;
      while( o )
	{
	  n_o = o->nObj;
	  free(o);
	  o = n_o;
	}
      free(r);
      r = n_r;
    }
  free(tick);
}

/*------------------------------------------------------*
 * AlarmHandler:                                        *
 *     Handler function used to profile regions.        *
 *------------------------------------------------------*/
void
AlarmHandler()
{
  timeToProfile = 1;
  signal(signalType, AlarmHandler);   // setup signal again
}

/*-------------------------------------------------------------------*
 * ProfileTick: Update the tick list by traversing all regions.      *
 *-------------------------------------------------------------------*/

static inline void
profileObj(ObjectDesc *fObj, ObjectList *newObj, RegionList *newRegion,
	   long *infiniteObjectUse, long *infiniteObjectDescUse)
{
  if ( lookupObjectListTable(fObj->atId) == NULL )
    {
      // Allocate new object
      newObj = (ObjectList *)allocMemProfiling_xx(sizeof(ObjectList));
      newObj->atId = fObj->atId;
      newObj->size = fObj->size;
      newObj->nObj = newRegion->fObj;
      newRegion->fObj = newObj;
      newRegion->used += fObj->size;
      newRegion->noObj++;
      insertObjectListTable(fObj->atId, newObj);
    }
  else
    {
      newObj = lookupObjectListTable(fObj->atId);
      newObj->size += fObj->size;
      newRegion->used += fObj->size;
    }
  *infiniteObjectUse += fObj->size;
  *infiniteObjectDescUse += sizeObjectDesc;
}

static inline void
profileGen(Gen *gen, ObjectList *newObj, RegionList *newRegion,
	   long *infiniteObjectUse, long *infiniteObjectDescUse,
	   long *infiniteRegionWaste)
{
  ObjectDesc *fObj;
  Rp *crp;                       /* Pointer to a region page. */

  /* Traverse objects in generation gen, except the last region page,
   * which is traversed independently; crp always points at the
   * beginning of a regionpage(=nPtr|dummy|data). */
  for( crp = clear_fp(gen->fp) ; crp->n ; crp = crp->n )
    {
      fObj = (ObjectDesc *) (((long *)crp)+HEADER_WORDS_IN_REGION_PAGE); // crp is a Rp
      // notPP = 0 means no object allocated
      while ( ((long *)fObj < ((long *)crp)+ALLOCATABLE_WORDS_IN_REGION_PAGE+HEADER_WORDS_IN_REGION_PAGE)
	      && (fObj->atId!=notPP) )
	{
	  profileObj(fObj,newObj,newRegion,infiniteObjectUse,infiniteObjectDescUse);
	  fObj=(ObjectDesc *)(((long*)fObj)+((fObj->size)+sizeObjectDesc)); // Find next object
	}
      newRegion->waste +=
	(long)((((long *)crp)+ALLOCATABLE_WORDS_IN_REGION_PAGE+HEADER_WORDS_IN_REGION_PAGE)-((long *)fObj));
      *infiniteRegionWaste +=
	(long)((((long *)crp)+ALLOCATABLE_WORDS_IN_REGION_PAGE+HEADER_WORDS_IN_REGION_PAGE)-((long *)fObj));
      /* No more objects in current region page. */
    }
  /* Now we need to traverse the last region page, now pointed
   * to by crp (crp is a Rp) */
  fObj = (ObjectDesc *) (((long *)crp)+HEADER_WORDS_IN_REGION_PAGE);

  while ( (uintptr_t *)fObj < gen->a )
    {
      profileObj(fObj,newObj,newRegion,infiniteObjectUse,infiniteObjectDescUse);
      fObj=(ObjectDesc *)(((size_t *)fObj)+((fObj->size)+sizeObjectDesc)); /* Find next object. */
    }
  newRegion->waste +=
    (size_t)((((size_t *)crp)+ALLOCATABLE_WORDS_IN_REGION_PAGE+HEADER_WORDS_IN_REGION_PAGE)-((size_t *)fObj));
  *infiniteRegionWaste +=
    (size_t)((((size_t *)crp)+ALLOCATABLE_WORDS_IN_REGION_PAGE+HEADER_WORDS_IN_REGION_PAGE)-((size_t *)fObj));

  /* No more objects in the last region page. */
}

void
profileTick(long *stackTop)
{
  TickList *newTick;
  FiniteRegionDesc *frd;
  ObjectDesc *fObj;
  ObjectList *newObj, *tempObj;
  RegionList *newRegion;
  Ro *rd;                        /* Used as pointer to infinite region. */

  long finiteRegionDescUse;       /* Words used on finite region descriptors. */
  long finiteObjectDescUse;       /* Words used on object descriptors in finite regions. */
  long finiteObjectUse;           /* Words used on objects in finite regions. */
  long infiniteObjectUse;         /* Words used on objects in infinite regions. */
  long infiniteObjectDescUse;     /* Words used on object descriptors in infinite regions. */
  long infiniteRegionWaste;       /* Words not used in region pages. */
  long regionDescUseProf;         /* Words used on extra fields in infinite region desc. when profiling. */

  /*  checkProfTab("profileTick.enter"); */

  doing_prof = 1; /* Mutex on profilig */
  debug(printf("Entering profileTick\n"));

  if ( profType == noTimer )
    {
      tempCount ++;
      if ( tempCount < profNo )
	return;
      tempCount = 0;
    }
  else
    {
      timeToProfile = 0; // We use timer so no profiling before next tick
    }

  if ( verboseProfileTick )
    {
      fprintf(stderr,"profileTick -- ENTER\n");
    }

  finiteRegionDescUse = 0;
  finiteObjectDescUse = 0;
  finiteObjectUse = 0;
  infiniteObjectUse = 0;
  infiniteObjectDescUse = 0;
  infiniteRegionWaste = 0;
  regionDescUseProf = 0;

  /* Allocate new tick. */
  newTick = (TickList *)allocMemProfiling_xx(sizeof(TickList));

  newTick->stackUse = stackBot - stackTop; //maybe +1
  maxStackP = (long*) min((unsigned long)maxStackP, (unsigned long)stackTop);

  //printf("Stackuse at entry %ld, stackbot: %p, stackTop: %p\n", newTick->stackUse, stackBot, stackTop);

  if ( newTick->stackUse < 0 )
    {
      sprintf(errorStr, "ERROR1 - PROFILE_TICK -- stackUse in profileTick less than zero %ld (bot %p, top %p)\n",
	      newTick->stackUse, stackBot, stackTop);
      profileERROR(errorStr);
    }

  newTick->regionDescUse = 0;
  cpuTimeAcc += (unsigned long)(((unsigned long)clock())-lastCpuTime);
  newTick->time = cpuTimeAcc;
  if ( tellTime == 1 )
    {
      fprintf(stderr,"The time is: %ld\n", cpuTimeAcc);
      tellTime = 0;
    }
  newTick->nTick   = NULL;
  newTick->fRegion = NULL;
  if (firstTick == NULL)
    firstTick = newTick;
  else
    lastTick->nTick = newTick;
  lastTick = newTick;

  /* Initialize hash table for regions. */
  initializeRegionListTable();

  /********************************/
  /* Traverse finite region list. */
  /********************************/

  for ( frd = topFiniteRegion ; frd ; frd = frd->p )
    {
      finiteRegionDescUse += sizeFiniteRegionDesc;
      finiteObjectDescUse += sizeObjectDesc;
      newTick->stackUse -= sizeFiniteRegionDesc;
      newTick->stackUse -= sizeObjectDesc;
      if (newTick->stackUse < 0)
	{
	  sprintf(errorStr, "ERROR2 - PROFILE_TICK -- stackUse in profileTick less than zero %ld\n",
		  newTick->stackUse);
	  profileERROR(errorStr);
	}
      fObj = (ObjectDesc *) (frd+1);

      //printf("FiniteRegionInfo: regionId: %ld, pPoint: %ld, size: %ld, stackuse: %ld, stacksize: %ld\n",
      //       frd->regionId, fObj->atId, fObj->size, newTick->stackUse,
      //       stackBot - stackTop); // 2001-05-11, Niels

      if ( fObj->size >= ALLOCATABLE_WORDS_IN_REGION_PAGE )
	{
	  sprintf(errorStr, "ERROR - PROFILE_TICK -- Size quite big, pp: %zu with size: %zu, fObj-1: %zu, fObj: %zu in finite region: %lu\n",
		  fObj->atId, fObj->size, *(((size_t*)fObj)-1), (size_t)fObj, frd->regionId);
	  profileERROR(errorStr);
	}

      newTick->stackUse -= fObj->size;

      //fprintf(stderr,"NOTE PROFILE_TICK -- stackUse: %ld, after object with size %zu, stackBot: %p, stackTop: %p\n",
      //        newTick->stackUse, fObj->size, stackBot, stackTop);

      finiteObjectUse += fObj->size;
      if ( newTick->stackUse < 0 )
	{
	  fprintf(stderr,"ERROR3 - PROFILE_TICK -- stackUse in profileTick less than \
             zero %ld, after object with size %zu and pp %zu, stackBot: %p, stackTop: %p\n",
		  newTick->stackUse, fObj->size, fObj->atId, stackBot, stackTop);
	  profileERROR(errorStr);
	}

      if ( lookupRegionListTable(frd->regionId) == NULL )
	{
	  newRegion = (RegionList *)allocMemProfiling_xx(sizeof(RegionList));
	  newRegion->regionId = frd->regionId;
	  newRegion->used = fObj->size;
	  newRegion->waste = 0;
	  newRegion->noObj = 1;
	  newRegion->infinite = 0;
	  newRegion->nRegion = newTick->fRegion;
	  newTick->fRegion = newRegion;;
	  newObj = (ObjectList *)allocMemProfiling_xx(sizeof(ObjectList));
	  newRegion->fObj = newObj;
	  newObj->atId = fObj->atId;
	  newObj->size = fObj->size;
	  newObj->nObj = NULL;
	  insertRegionListTable(frd->regionId, newRegion);
	}
      else
	{
	  newRegion = lookupRegionListTable(frd->regionId);
	  if ( newRegion->infinite )
	    {
	      // for check only
	      sprintf(errorStr, "ERROR - PROFILE_TICK -- finite region %3ld is allocated as infinite. \n",
		      newRegion->regionId);
	      profileERROR(errorStr);
	    }
	  newRegion->used += fObj->size;

	  /* See if object is already allocated. */
	  newObj = NULL;
	  for ( tempObj = newRegion->fObj ; tempObj && newObj == NULL ; tempObj = tempObj->nObj )
	    {
	      if (tempObj->atId == fObj->atId)
		newObj = tempObj;
	    }

	  if ( newObj == NULL )
	    {
	      // Allocate new object
	      newObj = (ObjectList *)allocMemProfiling_xx(sizeof(ObjectList));
	      newObj->atId = fObj->atId;
	      newObj->size = fObj->size;
	      newObj->nObj = newRegion->fObj;
	      newRegion->fObj = newObj;
	      newRegion->noObj++;
	    }
	  else
	    {
	      newObj->size += fObj->size;
	    }
	}
    }

  /**********************************/
  /* Traverse infinite region list. */
  /**********************************/

  for ( rd = TOP_REGION ; rd ; rd = rd->p )
    {
      //printf("INF REGION PROFILE_TICK -- stackUse: %ld, regionId: %ld\n",
      //       newTick->stackUse, rd->regionId);

      newTick->stackUse -= sizeRo;             // size of infinite region desc

      //printf("INF REGION PROFILE_TICK -- stackUse after subtract of sizeRo: %ld\n",
      //       newTick->stackUse);

      if (newTick->stackUse < 0)
	{
	  sprintf(errorStr, "ERROR4 -PROFILE_TICK -- stackUse in profileTick less than zero %ld\n",
		  newTick->stackUse);
	  profileERROR(errorStr);
	}
      newTick->regionDescUse += (sizeRo-sizeRoProf); // size of infinite region desc without prof
      regionDescUseProf += sizeRoProf;               // size of profiling fields in inf reg desc
      if ( lookupRegionListTable(rd->regionId) == NULL )
	{
	  newRegion = (RegionList *)allocMemProfiling_xx(sizeof(RegionList));
	  newRegion->regionId = rd->regionId;
	  newRegion->used = 0;
	  newRegion->waste = 0;
	  newRegion->noObj = 0;
	  newRegion->infinite = 1;
	  newRegion->nRegion = newTick->fRegion;
	  newTick->fRegion = newRegion;
	  newRegion->fObj = NULL;
	  insertRegionListTable(rd->regionId, newRegion);
	}
      else
	{
	  newRegion = lookupRegionListTable(rd->regionId);
	  if ( newRegion->infinite != 1 )
	    {
	      // For check only
	      sprintf(errorStr, "ERROR - PROFILE_TICK -- infinite region %3ld is allocated as finite. \n",
		      newRegion->regionId);
	      profileERROR(errorStr);
	    }
	}

      // Initialize hash table for objects
      initializeObjectListTable();

      for ( newObj = newRegion->fObj ; newObj ; newObj = newObj->nObj )
	{
	  insertObjectListTable(newObj->atId, newObj);
	}

      /* Traverse objects in current region, except the last region page,
       * which is traversed independently; crp always points at the
       * beginning of a regionpage(=nPtr|dummy|data). */
      profileGen(&(rd->g0),newObj,newRegion,&infiniteObjectUse,
		 &infiniteObjectDescUse,&infiniteRegionWaste);

      #ifdef ENABLE_GEN_GC
      profileGen(&(rd->g1),newObj,newRegion,&infiniteObjectUse,
		 &infiniteObjectDescUse,&infiniteRegionWaste);
      #endif /* ENABLE_GEN_GC */
    }

  lastCpuTime = (unsigned int)clock();

  if ( verboseProfileTick )
    {
      fprintf(stderr,"Memory use on the stack at time %ld (in bytes)\n", newTick->time);
      fprintf(stderr,"      Infinite region descriptors..........: %10ld\n", newTick->regionDescUse*(sizeof(long*)));
      fprintf(stderr,"      Objects allocated in finite regions..: %10ld\n", finiteObjectUse*(sizeof(long*)));
      fprintf(stderr,"      Other data on the stack..............: %10ld\n", newTick->stackUse*(sizeof(long*)));
      fprintf(stderr,"    Total allocated data by program........: %10ld\n\n",
	      (newTick->regionDescUse+finiteObjectUse+newTick->stackUse)*(sizeof(long*)));
      fprintf(stderr,"      Finite region descriptors............: %10ld\n", finiteRegionDescUse*(sizeof(long*)));
      fprintf(stderr,"      Prof. fields in infinite region desc.: %10ld\n", regionDescUseProf*(sizeof(long*)));
      fprintf(stderr,"      Object descriptors in finite regions.: %10ld\n", finiteObjectDescUse*(sizeof(long*)));
      fprintf(stderr,"    Total allocated data by profiler.......: %10ld\n", (finiteRegionDescUse+finiteObjectDescUse+regionDescUseProf)*(sizeof(long*)));
      fprintf(stderr,"  Total stack use..........................: %10ld\n",
	      (newTick->regionDescUse+finiteObjectUse+newTick->stackUse+finiteRegionDescUse+finiteObjectDescUse+regionDescUseProf)*(sizeof(long*)));

      if (((newTick->regionDescUse+finiteObjectUse+newTick->stackUse+
	    finiteRegionDescUse+finiteObjectDescUse+regionDescUseProf)*(sizeof(long*))) != (stackBot-stackTop)*(sizeof(long*)))
	fprintf(stderr,"ERROR -- stacksize error in ProfileTick\n");

      fprintf(stderr,"Memory use in regions at time %ld (in bytes)\n", newTick->time);
      fprintf(stderr,"    Objects allocated in infinite regions..: %10ld\n", infiniteObjectUse);
      fprintf(stderr,"    Object descriptors in infinite regions.: %10ld\n", infiniteObjectDescUse);
      fprintf(stderr,"    Total waste in region pages............: %10ld\n", infiniteRegionWaste);
      fprintf(stderr,"  Total memory allocated to region pages...: %10ld\n",
	      (infiniteObjectUse+infiniteObjectDescUse+infiniteRegionWaste)*(sizeof(long*)));
      if ( ((infiniteObjectUse+infiniteObjectDescUse+infiniteRegionWaste)*(sizeof(long*))) % ALLOCATABLE_WORDS_IN_REGION_PAGE != 0 )
	fprintf(stderr,"ERROR -- region page size error in profileTick\n");

      fprintf(stderr,"profileTick -- LEAVE\n");
    }

  outputProfileTick(newTick);
  freeTick(newTick);

  if ( profileON && profType != noTimer )
    {
      profiling_on();
    }

  doing_prof = 0;

  /*  checkProfTab("profileTick.exit"); */

  if (raised_exn_interupt_prof)
    raise_exn((uintptr_t)&exn_INTERRUPT);
  if (raised_exn_overflow_prof)
    raise_exn((uintptr_t)&exn_OVERFLOW);
}

/*-------------------------------------------------------------------*
 * PrintProfile: Print all collected data on screen.                 *
 *-------------------------------------------------------------------*/
void
printProfile(void)
{
  TickList *newTick;
  ObjectList *newObj;
  RegionList *newRegion;

  for ( newTick = firstTick ; newTick ; newTick = newTick->nTick )
    {
      fprintf(stderr,"Starting new tick.\n");
      for ( newRegion = newTick->fRegion ; newRegion ; newRegion = newRegion->nRegion )
	{
	  if ( newRegion->infinite )
	    {
	      fprintf(stderr,"  Infinite region: %3ld, used: %3ld, waste: %3ld, noObj: %3ld, Infinite: %3ld.\n",
		      newRegion->regionId, newRegion->used, newRegion->waste,
		      newRegion->noObj,newRegion->infinite);
	    }
	  else
	    {
	      fprintf(stderr,"  Finite region: %3ld, used: %3ld, waste: %3ld, noObj: %3ld, Infinite: %3ld.\n",
		      newRegion->regionId, newRegion->used, newRegion->waste,
		      newRegion->noObj,newRegion->infinite);
	    }
	  for ( newObj = newRegion->fObj ; newObj ; newObj = newObj->nObj )
	    {
	      fprintf(stderr,"    Starting new object with allocation point %3ld, and size %3ld.\n",
		      newObj->atId, newObj->size);
	    }
	}
    }
  return;
}

/*----------------------------------------------------------------*
 * OutputProfile:                                                 *
 * Output word data file with all collected data.                 *
 * Layout of file is as follows:                                  *
 *  maxRegion                                                     *
 *  noOfTicks,                                                    *
 *      noOfRegions, stackUse, regionDescUse, cpuTime             *
 *        regionId, used, waste, noOfObj, infinite                *
 *          allocationPoint, size                                 *
 *          |                                                     *
 *	    allocationPoint, size                                 *
 *        |                                                       *
 *        regionId, used, waste, noOfObj, infinite                *
 *          allocationPoint, size                                 *
 *          |                                                     *
 *	    allocationPoint, size                                 *
 *      |                                                         *
 *      noOfRegions, stackUse, regionDescUse, cpuTime             *
 *        regionId, used, waste, noOfObj, infinite                *
 *          allocationPoint, size                                 *
 *          |                                                     *
 *	    allocationPoint, size                                 *
 *        |                                                       *
 *        regionId, used, waste, noOfObj, infinite                *
 *          allocationPoint, size                                 *
 *          |                                                     *
 *	    allocationPoint, size                                 *
 *  |                                                             *
 * Here we put the profiling table profTab:                       *
 *  sizeProfTab,                                                  *
 *    regionId, MaxAlloc                                          *
 *    |                                                           *
 *    regionId, MaxAlloc                                          *
 *----------------------------------------------------------------*/

void
outputProfilePre(void)
{
  debug(printf("[outputProfilePre..."));

  if ( exportProfileDatafile )
    {
      if ((logFile = fopen((char *) &logName, "w")) == NULL) {
	fprintf(stderr,"Cannot open logfile.\n");
	exit(-1);
      }
    }

  putw(42424242, logFile); /* dummy maxAlloc, updated in outputProfilePost */
  putw(42424242, logFile); /* dummy noOfTicks, updated in outputProfilePost */

  noOfTickInFile = 0; /* Initialize counter tick-counter */

  debug(printf("]"));

  return;
}

void
outputProfileTick(TickList *tick)
{
  long noOfRegions;
  ObjectList *newObj;
  RegionList *newRegion;

  debug(printf("[outputProfileTick..."));

  if (exportProfileDatafile)
    {
      noOfTickInFile++; /* Increment no of tick-counter */
      noOfRegions = 0;
      for (newRegion = tick->fRegion ; newRegion ; newRegion = newRegion->nRegion )
	noOfRegions++;

      putw(noOfRegions, logFile);
      putw(tick->stackUse, logFile);
      putw(tick->regionDescUse, logFile);
      putw(tick->time, logFile);

      for (newRegion = tick->fRegion ; newRegion ; newRegion = newRegion->nRegion )
	{
	  putw(newRegion->regionId, logFile);
	  putw(newRegion->used, logFile);
	  putw(newRegion->waste, logFile);
	  putw(newRegion->noObj, logFile);
	  putw(newRegion->infinite, logFile);

	  for ( newObj = newRegion->fObj ; newObj ; newObj = newObj->nObj )
	    {
	      putw(newObj->atId, logFile);
	      putw(newObj->size, logFile);
	    }
	}
    }
  debug(printf("]"));
  return;
}

void
outputProfilePost(void)
{
  long i;
  ProfTabList* p;

  debug(printf("[outputProfilePost..."));

  /* Output profTab to log file. */
  putw(profTabSize(), logFile);
  for ( i = 0 ; i < PROF_HASH_TABLE_SIZE ; i++ )
    for (p=profHashTab[i]; p != NULL; p=p->next)
      {
	putw(p->regionId, logFile);
	putw(p->maxAlloc, logFile);
      }

  fseek(logFile, 0, SEEK_SET);    // seek to the beginning of file
  putw(maxAlloc, logFile);        // overwrite first two words
  putw(noOfTickInFile, logFile);
  fclose(logFile);
  debug(printf("]"));
  return;
}

/* Traverse generation and calculate number of allocated bytes and
   bytes used for profiling information */
void calcAllocInGen(Gen *gen,long *alloc, long *allocProf)
{
  ObjectDesc *fObj;
  Rp *crp;                       /* Pointer to a region page. */

  /* Traverse objects in generation gen, except the last region page,
   * which is traversed independently; crp always points at the
   * beginning of a regionpage(=nPtr|dummy|data). */
  for( crp = clear_fp(gen->fp) ; crp->n ; crp = crp->n )
    {
      fObj = (ObjectDesc *) (((size_t *)crp)+HEADER_WORDS_IN_REGION_PAGE); // crp is a Rp
      // notPP = 0 means no object allocated

      while ( ((size_t *)fObj < ((size_t *)crp)+ALLOCATABLE_WORDS_IN_REGION_PAGE+HEADER_WORDS_IN_REGION_PAGE)
	      && (fObj->atId!=notPP) )
	{
	  *alloc += fObj->size;
	  *allocProf += sizeObjectDesc;
	  fObj=(ObjectDesc *)(((size_t*)fObj)+((fObj->size)+sizeObjectDesc)); // Find next object
	}
      /* No more objects in current region page. */
    }

  /* Now we need to traverse the last region page, now pointed
   * to by crp (crp is a Rp) */
  fObj = (ObjectDesc *) (((size_t *)crp)+HEADER_WORDS_IN_REGION_PAGE);
  while ( (uintptr_t *)fObj < gen->a )
    {
      *alloc += fObj->size;
      *allocProf += sizeObjectDesc;
      fObj=(ObjectDesc *)(((long*)fObj)+((fObj->size)+sizeObjectDesc)); /* Find next object. */
    }
  /* No more objects in the last region page. */
  return;
}

#else /*PROFILING is not defined */

void
queueMark(StringDesc *str)
{
  return;
}

#endif /*PROFILING*/
