/*----------------------------------------------------------------*
 *                        Profiling                               *
 *----------------------------------------------------------------*/

/* Only include this file if PROFILING is defined... */

#ifdef PROFILING

#include <stdio.h>

#if defined(hpux)
#include <signal.h>      /* Used by the Profiler */
#include <time.h>        /* Used by the Profiler */
#elif defined(sun)
#include <signal.h>      /* Used by signal. */
#include <sys/time.h>    /* Used by setitimer. */
#endif

#include "Profiling.h"
#include "Region.h"
#include "Tagging.h"
#include "String.h"


/*----------------------------------------------------------------*
 * Global declarations                                            *
 *----------------------------------------------------------------*/
int *stackBot;
int timeToProfile;
int maxStack;         /* Max. stack size from check_stack. */
int *maxStackP=NULL;  /* Max. stack addr. from ProfileTick. */
int tempAntal; 
int tellTime;         /* 1, if the next profile tick should print out the
                         current time - 0 otherwise */

struct itimerval rttimer;    
struct itimerval old_rttimer;
int    profileON = TRUE; /* if false profiling is not started after a profileTick. */

char * freeProfiling;  /* Pointer to free-chunk of mem. to profiling data. */
int freeProfilingRest; /* Number of bytes left in freeProfiling-chunk.     */

TickList * firstTick; /* Pointer to data for the first tick. */
TickList * lastTick;  /* Pointer to data for the last tick. */
RegionList * regionListTable[MAX_REGIONS_TO_PROFILE]; /* Used as hash table into a region list. */
ObjectList * objectListTable[MAX_ALLOCATION_POINTS];  /* Used as hash table into an object list. */

unsigned int numberOfTics=0; /* Number of profilings so far. */

unsigned int lastCpuTime=0; /* CPU time after last tick. */
unsigned int cpuTimeAcc=0;  /* Used time by program excl. profiling. */

int noTimer =                                  /* Profile with a constant number of function calls. */
    ITIMER_REAL+ITIMER_VIRTUAL+ITIMER_PROF;    /* A number different from the other constants.      */
int profType = ITIMER_VIRTUAL; /* Type of profiling to use */
int signalType = SIGVTALRM;    /* Signal to catch depending on profType. */
int profNo = 10000;   
int microsec = 0;
int sec = 1;
int verboseProfileTick = 0;
int printProfileTab = 0;
int exportProfileDatafile = 1;
int showStat = 1;

char  logName[100]="profile.rp";   /* Name of log file to use. */
FILE* logFile;
char prgName[100];

static unsigned int max(unsigned int a, unsigned int b) {
  return (a<b)?b:a;
}

/*----------------------------------------------------------------------*
 *                        Statistical operations.                       *
 *----------------------------------------------------------------------*/

/* This function sets the flags 'tellTime' so that next time
   a tick is made, the time is printed on stdout */

void queueMark(StringDesc *str) {
    tellTime = tellTime;
}

void queueMarkProf(StringDesc *str, int pPoint){
    tellTime = 1;
    fprintf(stderr,"Reached ");
    printString(str);
}

/* This function is called each time a maximal stack size is reached. */
/* It is called from the assembler files (hpc#.s).                    */
void updateMaxProfStack() {

  maxProfStack = regionDescUseProfInf + regionDescUseProfFin + allocProfNowFin;

  return;
}

/* Calculate the the allocated and used space in a region. */
/* All instantiated regions with this region name is       */
/* calculated as one region.                               */
void AllocatedSpaceInARegion(Ro *rp)
{ unsigned  allokeret;

  if (rp->regionId > MAX_REGIONS_TO_PROFILE) {
    fprintf(stderr,"RegionId %d too large in AllocatedSpaceInRegion. Change constant MAX_REGIONS_TO_PROFILE, currently with value %d, in file Region.h and recompile the runtime system.\n", rp->regionId, MAX_REGIONS_TO_PROFILE);
    exit(-1);
  }
  allokeret = profTab[rp->regionId][NoOfPages]*ALLOCATABLE_WORDS_IN_REGION_PAGE;
  fprintf(stderr,"    Allocated space in regions with name %5d:%5d\n",rp->regionId, allokeret);
  return;
}

/* Prints all pages in the region. */
void PrintRegion(Ro* rp)
{ int i;
  Klump *ptr;
  
  if (rp!=NULL)
  {
    fprintf(stderr,"\nAddress of Ro %0x, First free word %0x, Border of region %0x\n     ",rp,rp->a,rp->b);
    for (ptr=rp->fp,i=1;ptr!=NULL; ptr=ptr->k.n,i++) {
      fprintf(stderr,"-->Page%2d:%d",i,ptr);
      if (i%3 == 0)
	fprintf(stderr,"\n     ");      
    }
    fprintf(stderr,"\n");
  }
}

void resetProfTab() {
  int i;

  for (i=0;i<MAX_REGIONS_TO_PROFILE;i++) {
    profTab[i][NoOfPages] = 0;
    profTab[i][MaxNoOfPages] = 0;
    profTab[i][NoOfInstances] = 0;
    profTab[i][MaxNoOfInstances] = 0;
    profTab[i][AllocNow] = 0;
    profTab[i][MaxAlloc] = 0;
  }

  return;
}

void resetProfiler() {

  resetProfTab();
  lastCpuTime = (unsigned int)clock();
  if (profType == noTimer)
    timeToProfile = 1;
  else {
    timeToProfile = 0;
    rttimer.it_value.tv_sec = sec;         /* Time in seconds to first tick. */
    rttimer.it_value.tv_usec = microsec;   /* Time in microseconds to first tick. */
    rttimer.it_interval.tv_sec = 0;        /* Time in seconds between succeding ticks. */
    rttimer.it_interval.tv_usec = 0;       /* Time in microseconds between succeding ticks. */

    signal(signalType, AlarmHandler);

    profiling_on(); 
  }
  fprintf(stderr,"\nProfiling is turned on with options:\n");
  if (profType == noTimer) {
    fprintf(stderr,"     profile timer is turned off.\n");
    fprintf(stderr,"     a profile tick occurs every %dth entrance to a function.\n", profNo);
  }
  if (profType == ITIMER_REAL) {
    fprintf(stderr,"     profile timer (unix real timer) is turned on.\n");
  }
  if (profType == ITIMER_VIRTUAL) {
    fprintf(stderr,"     profile timer (unix virtual timer) is turned on.\n");
  }
  if (profType == ITIMER_PROF) {
    fprintf(stderr,"     profile timer (unix profile timer) is turned on.\n");
  }
  if (microsec != 0 && profType != noTimer) {
    fprintf(stderr,"     a profile tick occurs every %dth microsecond.\n", microsec);
  }
  if (sec != 0 && profType != noTimer) {
    fprintf(stderr,"     a profile tick occurs every %dth second.\n", sec);
  }
  fprintf(stderr,"     profiling data is written on file %s.\n", logName);

  return;
}

void printProfTab() {
  int i;
  int noOfPagesTab, maxNoOfPagesTab;
  int noOfInstancesTab, maxNoOfInstancesTab;
  int allocNowTab, maxAllocTab;
  int noOfPagesTot, maxNoOfPagesTot;
  int noOfInstancesTot, maxNoOfInstancesTot;
  int allocNowTot, maxAllocTot;

  noOfPagesTot = 0;
  maxNoOfPagesTot = 0;
  noOfInstancesTot = 0;
  maxNoOfInstancesTot = 0;
  allocNowTot = 0;
  maxAllocTot = 0;

  fprintf(stderr,"\n\nPRINTING PROFILING TABLE.\n");
  for (i=0;i<MAX_REGIONS_TO_PROFILE;i++) {
    noOfPagesTab = profTab[i][NoOfPages];
    noOfPagesTot += noOfPagesTab;
    maxNoOfPagesTab = profTab[i][MaxNoOfPages];
    maxNoOfPagesTot += maxNoOfPagesTab;
    noOfInstancesTab = profTab[i][NoOfInstances];
    noOfInstancesTot += noOfInstancesTab;
    maxNoOfInstancesTab = profTab[i][MaxNoOfInstances];
    maxNoOfInstancesTot += maxNoOfInstancesTab;
    allocNowTab = profTab[i][AllocNow];
    allocNowTot += allocNowTab;
    maxAllocTab = profTab[i][MaxAlloc];
    maxAllocTot += maxAllocTab;
    if (maxNoOfPagesTab) 
      fprintf(stderr,"    profTab[rId%5d]: noOfPages = %8d, maxNoOfPages = %8d, noOfInstances = %8d, maxNoOfInstances = %8d, allocNow = %8d, maxAlloc = %8d\n",
	     i, noOfPagesTab, maxNoOfPagesTab, noOfInstancesTab, maxNoOfInstancesTab, allocNowTab*4, maxAllocTab*4);
  }
  fprintf(stderr,"    -----------------------------------------------------------------------------------------------------------------------------------------------------------------\n");
  fprintf(stderr,"                                   %8d     SUM OF MAX: %8d                  %8d         SUM OF MAX: %8d      Bytes: %8d      Bytes: %8d\n",
	 noOfPagesTot, maxNoOfPagesTot, noOfInstancesTot, maxNoOfInstancesTot, allocNowTot*4, maxAllocTot*4);
  fprintf(stderr,"    =================================================================================================================================================================\n");

}

void Statistics()
{ Klump *ptr;
  int i,ii;
  double Mb = 1024.0*1024.0;

  if (showStat) {
    fprintf(stderr,"\n*************Region statistics***************\n\n");



#if FREESTAT
    fprintf(stderr,"\nFree, Address %0x\n     ",freelist);
    ptr = freelist;
    ii = 1;
    while (ptr->k.n != NULL) {
      ptr = ptr->k.n;
      fprintf(stderr,"-->Page %3d:%0x",ii,ptr);
      if (ii%3 == 0)
	fprintf(stderr,"\n     ");
      ii++;
    }
    fprintf(stderr,"\n\n     ");
#endif

    if (printProfileTab) 
      printProfTab();

    fprintf(stderr,"\nSBRK.\n");
    fprintf(stderr,"  Number of calls to sbrk                     : %10d\n",callsOfSbrk);
    fprintf(stderr,"  Number of bytes allocated in each SBRK call : %10d\n", BYTES_ALLOC_BY_SBRK);
    fprintf(stderr,"  Total number of bytes allocated by SBRK     : %10d (%3.1fMb)\n", BYTES_ALLOC_BY_SBRK*callsOfSbrk,
	   (BYTES_ALLOC_BY_SBRK*callsOfSbrk)/Mb );

    fprintf(stderr,"\nREGIONPAGES.\n");
    fprintf(stderr,"  Size of one page              : %10d bytes\n",ALLOCATABLE_WORDS_IN_REGION_PAGE*4);

    fprintf(stderr,"\n  Max. no. of simultaneously allocated pages : %10d\n",maxNoOfPages);
    fprintf(stderr,"  Number of allocated pages now              : %10d\n",noOfPages);

    fprintf(stderr,"\nREGIONS.\n");
    fprintf(stderr,"  Size of infinite region descriptor (incl. profiling information) : %10d bytes\n",sizeRo*4);
    fprintf(stderr,"  Size of infinite region descriptor (excl. profiling information) : %10d bytes\n",(sizeRo-sizeRoProf)*4);

    fprintf(stderr,"\n  Size of finite region descriptor   : %10d bytes\n",sizeof(FiniteRegionDesc));

    fprintf(stderr,"\n  Number of calls to allocateRegionInf   : %10d\n",callsOfAllocateRegionInf);
    fprintf(stderr,"  Number of calls to deallocateRegionInf : %10d\n",callsOfDeallocateRegionInf);

    fprintf(stderr,"\n  Number of calls to allocateRegionFin   : %10d\n",callsOfAllocateRegionFin);
    fprintf(stderr,"  Number of calls to deallocateRegionFin : %10d\n",callsOfDeallocateRegionFin);
    
    fprintf(stderr,"\n  Number of calls to alloc                  : %10d\n",callsOfAlloc);
    fprintf(stderr,"  Number of calls to resetRegion            : %10d\n",callsOfResetRegion);
    fprintf(stderr,"  Number of calls to deallocateRegionsUntil : %10d\n",callsOfDeallocateRegionsUntil);

    fprintf(stderr,"\n  Max. no. of co-existing regions (finite plus infinite) : %10d\n",maxNoOfInstances);
    fprintf(stderr,"  Number of regions now                                  : %10d\n",noOfInstances);

    fprintf(stderr,"\n  Live data in infinite regions : %10d bytes (%4.1fMb)\n", allocNowInf*4, (allocNowInf*4)/Mb);
    fprintf(stderr,"  Live data in finite regions   : %10d bytes (%4.1fMb)\n", allocNowFin*4, (allocNowFin*4)/Mb);
    fprintf(stderr,"  ---------------------------------------------------------\n");
    fprintf(stderr,"  Total live data               : %10d bytes (%4.1fMb)\n",(allocNowInf+allocNowFin)*4,((allocNowInf+allocNowFin)*4)/Mb);

    fprintf(stderr,"\n  Maximum space used for region pages                 : %10d bytes (%4.1fMb)\n", maxNoOfPages*ALLOCATABLE_WORDS_IN_REGION_PAGE*4,
	   (maxNoOfPages*ALLOCATABLE_WORDS_IN_REGION_PAGE*4)/Mb);
    fprintf(stderr,"  Maximum space used on data in region pages          : %10d bytes (%4.1fMb)\n", maxAllocInf*4,(maxAllocInf*4)/Mb);
    fprintf(stderr,"      Space in regions at that time used on profiling : %10d bytes (%4.1fMb)\n", maxAllocProfInf*4,
	   (maxAllocProfInf*4)/Mb);
    fprintf(stderr,"  -------------------------------------------------------------------------------\n");
    fprintf(stderr,"  Maximum allocated space in region pages             : %10d bytes (%4.1fMb)\n", (maxAllocProfInf+maxAllocInf)*4,
	   ((maxAllocProfInf+maxAllocInf)*4)/Mb);
    fprintf(stderr,"\n  Memory utilisation for infinite regions (%10d/%10d) : %2.0f%%\n",
	   (maxAllocProfInf+maxAllocInf)*4,
	   (maxNoOfPages*ALLOCATABLE_WORDS_IN_REGION_PAGE*4),
	   ((maxAllocProfInf+maxAllocInf)*4.0)/(maxNoOfPages*ALLOCATABLE_WORDS_IN_REGION_PAGE*4.0)*100.0);

    fprintf(stderr,"\n  Maximum space used on the stack for infinite region descriptors   : %10d bytes (%4.1fMb)\n", maxRegionDescUseInf*4,
	   (maxRegionDescUseInf*4)/Mb);
    fprintf(stderr,"      Additional space used on profiling information at that time   : %10d bytes (%4.1fMb)\n", maxRegionDescUseProfInf*4,
	   (maxRegionDescUseProfInf*4)/Mb);
    fprintf(stderr,"  ---------------------------------------------------------------------------------------------\n");
    fprintf(stderr,"  Maximum space used on infinite region descriptors on the stack    : %10d bytes (%4.1fMb)\n", 
	   (maxRegionDescUseInf+maxRegionDescUseProfInf)*4,((maxRegionDescUseInf+maxRegionDescUseProfInf)*4)/Mb);


    fprintf(stderr,"\n  Maximum space used on the stack for finite regions              : %10d bytes (%4.1fMb)\n", maxAllocFin*4,
	   (maxAllocFin*4)/Mb);
    fprintf(stderr,"      Additional space used on profiling information at that time : %10d bytes (%4.1fMb)\n", 
	   (maxAllocProfFin+maxRegionDescUseProfFin)*4, ((maxAllocProfFin+maxRegionDescUseProfFin)*4)/Mb);
    fprintf(stderr,"  -------------------------------------------------------------------------------------------\n");
    fprintf(stderr,"  Maximum space used on finite regions on the stack               : %10d bytes (%4.1fMb)\n", 
	   (maxAllocFin+maxAllocProfFin+maxRegionDescUseProfFin)*4,((maxAllocFin+maxAllocProfFin+maxRegionDescUseProfFin)*4)/Mb);

    fprintf(stderr,"\n  Max. size of stack when program was executed        : %10d bytes (%4.1fMb)\n", ((int)maxStack)-((int)stackBot),
	   (((int)maxStack)-((int)stackBot))/Mb);
    fprintf(stderr,"    Space used on profiling information at that time  : %10d bytes (%4.1fMb)\n", maxProfStack*4, (maxProfStack*4)/Mb);
    fprintf(stderr,"  -------------------------------------------------------------------------------\n");
    fprintf(stderr,"  Max. stack use excl. profiling information          : %10d bytes (%4.1fMb)\n",
	   ((int)maxStack)-((int)stackBot)-(maxProfStack*4), (((int)maxStack)-((int)stackBot)-(maxProfStack*4))/Mb);
    
    fprintf(stderr,"\n  Max. size of stack in a profile tick                : %10d bytes (%4.1fMb)\n", ((int)maxStackP)-((int)stackBot),
	   (((int)maxStackP)-((int)stackBot))/Mb);

    fprintf(stderr,"\n*********End of region statistics*********\n\n\n");
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
void profileERROR(char *errorStr) {
  fprintf(stderr,"\n***********************ERROR*****************************\n");
  fprintf(stderr,"%s\n", errorStr);
  fprintf(stderr,"\n***********************ERROR*****************************\n");
  exit(-1);
}

/*-----------------------------------------------*
 * This function prints the contents of a finite *
 * region on screen.                             *
 *-----------------------------------------------*/
void pp_finite_region (FiniteRegionDesc *frd) {
  ObjectDesc *obj;
  obj = (ObjectDesc *) (frd+1);
  fprintf(stderr,"FRDid: %d, next: %d, objectId: %d, objSize: %d\n",
	 frd->regionId, (int)frd, obj->atId, obj->size);
  return;
}

/*--------------------------------------------------*
 * This function prints the contents of an infinite *
 * region on screen.                                *
 *--------------------------------------------------*/
void pp_infinite_region (int rAddr) {
  ObjectDesc *fObj;
  Klump *crp;
  Ro *rp;
  rp = (Ro *) clearStatusBits(rAddr);
  for(crp=rp->fp; crp != NULL; crp=crp->k.n) {
    fObj = (ObjectDesc *) (((int *)crp)+HEADER_WORDS_IN_REGION_PAGE); /* crp is a Klump. */
    while ( ((int *)fObj < ((int *)crp)+ALLOCATABLE_WORDS_IN_REGION_PAGE+HEADER_WORDS_IN_REGION_PAGE) && (fObj->atId!=notPP) ) {
      fprintf(stderr,"ObjAtId %d, Size: %d\n", fObj->atId, fObj->size);
      fObj=(ObjectDesc *)(((int*)fObj)+((fObj->size)+sizeObjectDesc)); /* Find next object. */
    }
  }
  return;
}

/*---------------------------------------------------*
 * This function prints the contents of all infinite *
 * regions on screen.                                *
 *---------------------------------------------------*/
void pp_infinite_regions () {
  ObjectDesc *fObj;
  Klump *crp;
  Ro *rp;

  for (rp=topRegion;rp!=NULL;rp=rp->p) {
    fprintf(stderr,"Region %d\n", rp->regionId);
    for(crp=rp->fp; crp!=NULL; crp=crp->k.n) {
      fObj = (ObjectDesc *) (((int *)crp)+HEADER_WORDS_IN_REGION_PAGE); /* crp is a Klump. */
      while ( ((int *)fObj < ((int *)crp)+ALLOCATABLE_WORDS_IN_REGION_PAGE+HEADER_WORDS_IN_REGION_PAGE) && (fObj->atId!=notPP) ) {
	fprintf(stderr,"ObjAtId %d, Size: %d\n", fObj->atId, fObj->size);
	fObj=(ObjectDesc *)(((int*)fObj)+((fObj->size)+sizeObjectDesc)); /* Find next object. */
      }
    }
  }
  return;
}


/*---------------------------------------------------*
 * This function checks the contents of all infinite *
 * regions and prints any errors on screen.          *
 *---------------------------------------------------*/
void check_infinite_regions (int blockLab) {
  ObjectDesc *fObj;
  Klump *crp;
  Ro *rp;

  for (rp=topRegion;rp!=NULL;rp=rp->p) {
    if (rp->regionId < 0 || rp->regionId > MAX_REGIONS_TO_PROFILE) {
      sprintf(errorStr, "Wrong regionId info (found in block %d), %d\n", rp->regionId, blockLab);
      profileERROR(errorStr);
    }
    for(crp=rp->fp; crp!=NULL; crp=crp->k.n) {
      fObj = (ObjectDesc *) (((int *)crp)+HEADER_WORDS_IN_REGION_PAGE); /* crp is a Klump. */
      while ( ((int *)fObj < ((int *)crp)+ALLOCATABLE_WORDS_IN_REGION_PAGE+HEADER_WORDS_IN_REGION_PAGE) && (fObj->atId!=notPP) ) {
	if (fObj->atId < 2 || fObj->atId > MAX_ALLOCATION_POINTS) {
	  sprintf(errorStr, "Wrong object atId info, atId: %d, Size: %d, Region: %d, found in block %d\n", fObj->atId, fObj->size, rp->regionId, blockLab);
	  profileERROR(errorStr);
	}
	if (fObj->size < 1 || fObj->size > ALLOCATABLE_WORDS_IN_REGION_PAGE) {
	  sprintf(errorStr, "Wrong object size info, atId: %d, Size: %d, Region: %d, found in block %d\n", fObj->atId, fObj->size, rp->regionId, blockLab);
	  profileERROR(errorStr);
	}
	fObj=(ObjectDesc *)(((int*)fObj)+((fObj->size)+sizeObjectDesc)); /* Find next object. */
      }
    }
  }
/*  printf ("Check infinite regions, ok.\n");*/
  return;
}

/*------------------------------------------------------*
 * profiling_on                                         *
 *   Sets alarm for profiling.                          *
 *------------------------------------------------------*/
void profiling_on() {
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
void profiling_off() {
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
 * callSbrkProfiling()                                  *
 *   Calls sbrk for more memory to profiling data.      *
 *------------------------------------------------------*/
void callSbrkProfiling() {
  freeProfiling = (char *)malloc(BYTES_ALLOC_TO_PROFILING);
  if (freeProfiling == (char *)-1) {
    perror("SBRK error in callSbrkProfiling\n");
    exit(-1);
  }
  if (((int)freeProfiling) % 4)
    fprintf(stderr,"freeProfiling not aligned\n");
  freeProfilingRest = BYTES_ALLOC_TO_PROFILING;
  return;	
}

/*------------------------------------------------------*
 * allocMemProfiling                                    *
 *   Takes i bytes from the free-chunk of memory        *
 *   allocated for profiling data.                      *
 *------------------------------------------------------*/
char *allocMemProfiling(int i) {
  char * tempPtr;

  if (i > freeProfilingRest)
    callSbrkProfiling();

  if (i <= freeProfilingRest) {
    freeProfilingRest -= i;
    tempPtr = freeProfiling;
    freeProfiling += i;
  } else {
    fprintf(stderr,"ERROR in allocMemProfiling with freeProfilingRest=%d and i=%d\n",
	   freeProfilingRest, i);
    exit(-1);
  }
  return tempPtr;
}

/*------------------------------------------------------*
 * AlarmHandler:                                        *
 *     Handler function used to profile regions.        *
 *------------------------------------------------------*/
void AlarmHandler() {
#if DEBUG_ALARM_HANDLER_PROFILING
  fprintf(stderr,"AlarmHandler --- ENTER\n");
#endif

  timeToProfile = 1;

  signal(signalType, AlarmHandler); /* Setup signal again. */

#if DEBUG_ALARM_HANDLER_PROFILING
  fprintf(stderr,"AlarmHandler --- LEAVE\n");
#endif

  return;
}

/*-------------------------------------------------------------------*
 * ProfileTick: Update the tick list by traversing all regions.      *
 *-------------------------------------------------------------------*/
void profileTick(int *stackTop) {
  int i;
  int prevWaste;                 /* waste in prev region page, only used for check. */
  TickList *newTick;
  FiniteRegionDesc *frd;
  ObjectDesc *fObj;
  ObjectList *newObj, *tempObj;
  RegionList *newRegion;
  Ro *rd;                        /* Used as pointer to infinite region. */
  Klump *crp;                    /* Pointer to a region page. */

  int finiteRegionDescUse;       /* Words used on finite region descriptors. */
  int finiteObjectDescUse;       /* Words used on object descriptors in finite regions. */
  int finiteObjectUse;           /* Words used on objects in finite regions. */
  int infiniteObjectUse;         /* Words used on objects in infinite regions. */
  int infiniteObjectDescUse;     /* Words used on object descriptors in infinite regions. */
  int infiniteRegionWaste;       /* Words not used in region pages. */
  int regionDescUseProf;         /* Words used on extra fields in infinite region desc. when profiling. */

  if (profType == noTimer) {
    tempAntal ++;
    if (tempAntal < profNo)
      return;
    tempAntal = 0;
  } else
    timeToProfile = 0; /* We use timer so no profiling before next tick. */
  
  if (verboseProfileTick)
    fprintf(stderr,"profileTick -- ENTER\n");

  finiteRegionDescUse = 0;
  finiteObjectDescUse = 0;
  finiteObjectUse = 0;
  infiniteObjectUse = 0;
  infiniteObjectDescUse = 0;
  infiniteRegionWaste = 0;
  regionDescUseProf = 0;

  /* Allocate new tick. */
  newTick = (TickList *)allocMemProfiling(sizeof(TickList));
  newTick->stackUse = ((int *)stackTop)-((int *)stackBot);
  if (newTick->stackUse < 0) {
    sprintf(errorStr, "ERROR1 - PROFILE_TICK -- stackUse in profileTick less than zero %d\n",
	    newTick->stackUse);
    profileERROR(errorStr);
  }
  maxStackP = (int *) max((int)maxStackP, (int)stackTop);

  newTick->regionDescUse = 0;
  cpuTimeAcc += (unsigned int)(((unsigned int)clock())-lastCpuTime);
  newTick->time = cpuTimeAcc;
  if (tellTime == 1) {
    fprintf(stderr,"The time is: %d\n", cpuTimeAcc);
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
  for (i=0;i<MAX_REGIONS_TO_PROFILE;i++)
    regionListTable[i] = NULL;

  /********************************/
  /* Traverse finite region list. */
  /********************************/

#if DEBUG_PROFILE_TICK
  fprintf(stderr,"  Traversing finite region list.\n");
#endif

  for (frd=topFiniteRegion;frd!=NULL;frd=frd->p) {

#if DEBUG_PROFILE_TICK
  pp_finite_region(frd);
#endif

    finiteRegionDescUse += sizeFiniteRegionDesc;
    finiteObjectDescUse += sizeObjectDesc;
    newTick->stackUse -= sizeFiniteRegionDesc;
    newTick->stackUse -= sizeObjectDesc;
    if (newTick->stackUse < 0) {
      sprintf(errorStr, "ERROR2 - PROFILE_TICK -- stackUse in profileTick less than zero %d\n",
	      newTick->stackUse);
      profileERROR(errorStr);
    }
    fObj = (ObjectDesc *) (frd+1);

    if (fObj->atId >= MAX_ALLOCATION_POINTS) {
      sprintf(errorStr, "ERROR - PROFILE_TICK -- Program point too big %d with \
              size %d, fObj+2: %d, fObj %d in finite region %d\n", 
	      fObj->atId, fObj->size, *(((int*)fObj)+2),  (int)fObj, frd->regionId);
      profileERROR(errorStr);
    }
    
    if (fObj->size >= ALLOCATABLE_WORDS_IN_REGION_PAGE) {
      sprintf(errorStr, "ERROR - PROFILE_TICK -- Size quite big, pp: %d with  \
              size %d, fObj-1: %d, fObj %d in finite region %d\n", 
	      fObj->atId, fObj->size, *(((int*)fObj)-1), (int)fObj, frd->regionId);
      profileERROR(errorStr);
    }
    
    newTick->stackUse -= fObj->size;
    finiteObjectUse += fObj->size;
    if (newTick->stackUse < 0) {
      fprintf(stderr,"ERROR3 - PROFILE_TICK -- stackUse in profileTick less than \
             zero %d, after object with size %d and pp %d\n",
	     newTick->stackUse, fObj->size, fObj->atId);
      profileERROR(errorStr);
    }

    if (regionListTable[frd->regionId] == NULL) {
      newRegion = (RegionList *)allocMemProfiling(sizeof(RegionList));
      newRegion->regionId = frd->regionId;
      newRegion->numberOfInstances=1;
      newRegion->used = fObj->size;                   
      newRegion->waste = 0;                  
      newRegion->noObj = 1;
      newRegion -> infinite = 0;
      newRegion->nRegion = newTick->fRegion;
      newTick->fRegion = newRegion;;
      newObj = (ObjectList *)allocMemProfiling(sizeof(ObjectList));
      newRegion->fObj = newObj;
      newObj->atId = fObj->atId;
      newObj->size = fObj->size;
      newObj->numberOfInstances = 1;
      newObj->nObj = NULL;
      regionListTable[frd->regionId] = newRegion;
    } else {
      newRegion = regionListTable[frd->regionId];
      if (newRegion->infinite != 0) { /* For check only. */
	sprintf(errorStr, "ERROR - PROFILE_TICK -- finite region %3d is allocated as infinite. \n",
		newRegion->regionId);
	profileERROR(errorStr);
      }
      newRegion->numberOfInstances++;
      newRegion->used += fObj->size;

      /* See if object already allocated. */
      newObj = NULL;
      for (tempObj=newRegion->fObj;tempObj!=NULL && newObj == NULL;tempObj=tempObj->nObj)
	if (tempObj->atId == fObj->atId)
	  newObj = tempObj;

      if (newObj == NULL) {

#if PRINT_WARNINGS_PROFILE_TICK
	fprintf(stderr,"Warning - Finite region %d with two different objects %d, %d.\n",
	       newRegion->regionId, newRegion->fObj->atId, fObj->atId);
#endif

	/* Allocate new object. */
	newObj = (ObjectList *)allocMemProfiling(sizeof(ObjectList));
	newObj->atId = fObj->atId;
	newObj->size = fObj->size;
	newObj->numberOfInstances = 1;
	newObj->nObj = newRegion->fObj;
	newRegion->fObj = newObj;
	newRegion->noObj++;
      } else {
	newObj->size += fObj->size;
	newObj->numberOfInstances++;
      }
    }
  }


  /**********************************/
  /* Traverse infinite region list. */
  /**********************************/

#if DEBUG_PROFILE_TICK
  fprintf(stderr,"  Traversing infinite region list.\n");
#endif

  for (rd=topRegion;rd!=NULL;rd=rd->p) {

#if DEBUG_PROFILE_TICK
  /*fprintf(stderr," rd: %10d and rd->p: %10d\n", rd, rd->p);*/
  fprintf(stderr,"region id: %d\n",rd->regionId);

if (rd->regionId == 11110)
  allocProfiling((int)rd,4,42);
#endif

    newTick->stackUse -= sizeRo; /* Size of infinite region desc. */
    if (newTick->stackUse < 0) {
      sprintf(errorStr, "ERROR4 -PROFILE_TICK -- stackUse in profileTick less than zero %d\n",
	      newTick->stackUse);
      profileERROR(errorStr);
    }
    newTick->regionDescUse += (sizeRo-sizeRoProf); /* Size of infinite region desc. without prof. */
    regionDescUseProf += sizeRoProf;               /* Size of profiling fields in inf. reg. desc. */
    if (regionListTable[rd->regionId] == NULL) {
      newRegion = (RegionList *)allocMemProfiling(sizeof(RegionList));
      newRegion->regionId = rd->regionId;
      newRegion->numberOfInstances=1;
      newRegion->used = 0;
      newRegion->waste = 0;                  
      newRegion->noObj = 0;
      newRegion->infinite = 1;
      newRegion->nRegion = newTick->fRegion;
      newTick->fRegion = newRegion;
      newRegion->fObj = NULL;
      regionListTable[rd->regionId] = newRegion;
    } else {
      newRegion = regionListTable[rd->regionId];
      if (newRegion->infinite != 1) { /* For check only. */
	sprintf(errorStr, "ERROR - PROFILE_TICK -- infinite region %3d is allocated as finite. \n",
		newRegion->regionId);
	profileERROR(errorStr);
      }
      newRegion->numberOfInstances++;
    }

    /* Initialize hash table for objects. */
    for (i=0;i<MAX_ALLOCATION_POINTS;i++)
      objectListTable[i] = NULL;
    for (newObj=newRegion->fObj;newObj!=NULL;newObj=newObj->nObj)
      objectListTable[newObj->atId]=newObj;

    /* Traverse objects in current region. */
    /* crp always points at the beginning of a regionpage(=nPtr|dummy|data). */

#if DEBUG_PROFILE_TICK
  /*fprintf(stderr,"Traverse objects in current region. - ENTER.\n");*/
#endif

    prevWaste = 0;
    for(crp=rd->fp; crp != NULL; crp=crp->k.n) {
      fObj = (ObjectDesc *) (((int *)crp)+HEADER_WORDS_IN_REGION_PAGE); /* crp is a Klump. */
       if (fObj->size+sizeObjectDesc < prevWaste) {
	fprintf(stderr,"ERROR - PROFILE_TICK -- first object with size %3d \
               could be placed in previous region page with waste %3d.\n",
	       fObj->size+sizeObjectDesc, prevWaste);
	fprintf(stderr,"                     -- crp %d, rp_size in words %d, fObj %d\n",
	       (int)crp, ALLOCATABLE_WORDS_IN_REGION_PAGE+HEADER_WORDS_IN_REGION_PAGE, (int)fObj);
	exit(-1);
      }
      /* notPP = 0 means no object allocated. */

#if DEBUG_PROFILE_TICK
  /*fprintf(stderr,"Traverse objects in current region page %d. - ENTER.\n", (int) crp);*/
#endif

      while ( ((int *)fObj < ((int *)crp)+ALLOCATABLE_WORDS_IN_REGION_PAGE+HEADER_WORDS_IN_REGION_PAGE) && (fObj->atId!=notPP) ) {
	if (fObj->atId >= MAX_ALLOCATION_POINTS) {
	  sprintf(errorStr, "ERROR - PROFILE_TICK -- Program point too big %d with \
                  size %d, fObj+2: %d, fObj %d and crp+rpsize %d in region %d\n", 
		  fObj->atId, fObj->size, *(((int*)fObj)+2),  
		  (int)fObj, ((int *)crp)+ALLOCATABLE_WORDS_IN_REGION_PAGE, newRegion->regionId);
	  profileERROR(errorStr);
	}

	if (fObj->size >= ALLOCATABLE_WORDS_IN_REGION_PAGE) {
	  sprintf(errorStr, "ERROR - PROFILE_TICK -- Size quite big, pp: %d with \
                  size %d, fObj-1: %d, fObj %d and crp+rpsize %d in infinite region %d\n", 
		  fObj->atId, fObj->size, *(((int*)fObj)-1),
		  (int)fObj, ((int *)crp)+ALLOCATABLE_WORDS_IN_REGION_PAGE, newRegion->regionId);
	  profileERROR(errorStr);
	}

	if (objectListTable[fObj->atId] == NULL) {
	  /* Allocate new object. */
	  newObj = (ObjectList *)allocMemProfiling(sizeof(ObjectList));
	  newObj->atId = fObj->atId;
	  newObj->size = fObj->size;
	  newObj->numberOfInstances = 1;
	  newObj->nObj = newRegion->fObj;
	  newRegion->fObj = newObj;
	  newRegion->used += fObj->size;
	  newRegion->noObj++;
	  objectListTable[fObj->atId] = newObj;
	} else {
	  newObj = objectListTable[fObj->atId];
	  newObj->size += fObj->size;
	  newObj->numberOfInstances++;
	  newRegion->used += fObj->size;
	}
	infiniteObjectUse += fObj->size;
	infiniteObjectDescUse += sizeObjectDesc;
	fObj=(ObjectDesc *)(((int*)fObj)+((fObj->size)+sizeObjectDesc)); /* Find next object. */
      }
      newRegion->waste += (int)((((int *)crp)+ALLOCATABLE_WORDS_IN_REGION_PAGE+HEADER_WORDS_IN_REGION_PAGE)-((int *)fObj));
      prevWaste = (int)((((int *)crp)+ALLOCATABLE_WORDS_IN_REGION_PAGE+HEADER_WORDS_IN_REGION_PAGE)-((int *)fObj)); /* For check only. */
      infiniteRegionWaste += (int)((((int *)crp)+ALLOCATABLE_WORDS_IN_REGION_PAGE+HEADER_WORDS_IN_REGION_PAGE)-((int *)fObj));
      /* No more objects in current region page. */

#if DEBUG_PROFILE_TICK
  /*fprintf(stderr,"\nTraverse objects in current region page %d, next %d. - LEAVE.\n", (int) crp, crp->k.n);*/
#endif

    }

#if DEBUG_PROFILE_TICK
  /*fprintf(stderr,"Traverse objects in current region. - LEAVE.\n");*/
#endif

  }

  lastCpuTime = (unsigned int)clock();

  if (verboseProfileTick) {

    fprintf(stderr,"Memory use on the stack at time %d (in bytes)\n", newTick->time);
    fprintf(stderr,"      Infinite region descriptors..........: %10d\n", newTick->regionDescUse*4);
    fprintf(stderr,"      Objects allocated in finite regions..: %10d\n", finiteObjectUse*4);
    fprintf(stderr,"      Other data on the stack..............: %10d\n", newTick->stackUse*4);
    fprintf(stderr,"    Total allocated data by program........: %10d\n\n",
	   (newTick->regionDescUse+finiteObjectUse+newTick->stackUse)*4);
    fprintf(stderr,"      Finite region descriptors............: %10d\n", finiteRegionDescUse*4);
    fprintf(stderr,"      Prof. fields in infinite region desc.: %10d\n", regionDescUseProf*4);
    fprintf(stderr,"      Object descriptors in finite regions.: %10d\n", finiteObjectDescUse*4);
    fprintf(stderr,"    Total allocated data by profiler.......: %10d\n", (finiteRegionDescUse+finiteObjectDescUse+regionDescUseProf)*4);
    fprintf(stderr,"  Total stack use..........................: %10d\n",
	   (newTick->regionDescUse+finiteObjectUse+newTick->stackUse+finiteRegionDescUse+finiteObjectDescUse+regionDescUseProf)*4);
    if (((newTick->regionDescUse+finiteObjectUse+newTick->stackUse+
	  finiteRegionDescUse+finiteObjectDescUse+regionDescUseProf)*4) != (stackTop-stackBot)*4)
      fprintf(stderr,"ERROR -- stacksize error in ProfileTick\n");
    fprintf(stderr,"Memory use in regions at time %d (in bytes)\n", newTick->time);
    fprintf(stderr,"    Objects allocated in infinite regions..: %10d\n", infiniteObjectUse);
    fprintf(stderr,"    Object descriptors in infinite regions.: %10d\n", infiniteObjectDescUse);
    fprintf(stderr,"    Total waste in region pages............: %10d\n", infiniteRegionWaste);
    fprintf(stderr,"  Total memory allocated to region pages...: %10d\n", 
	   (infiniteObjectUse+infiniteObjectDescUse+infiniteRegionWaste)*4);
    if ( ((infiniteObjectUse+infiniteObjectDescUse+infiniteRegionWaste)*4) % ALLOCATABLE_WORDS_IN_REGION_PAGE != 0 )
      fprintf(stderr,"ERROR -- region page size error in profileTick\n");
      

    fprintf(stderr,"profileTick -- LEAVE\n");
  }

  if (profType != noTimer && profileON) {
    profiling_on();
  }

}

/*-------------------------------------------------------------------*
 * PrintProfile: Print all collected data on screen.                 *
 *-------------------------------------------------------------------*/
void printProfile(void) {
  TickList *newTick;
  ObjectList *newObj;
  RegionList *newRegion;

  for (newTick=firstTick;newTick!=NULL;newTick=newTick->nTick) {
    fprintf(stderr,"Starting new tick.\n");
    for (newRegion=newTick->fRegion;newRegion!=NULL;newRegion=newRegion->nRegion) {
      if (newRegion->infinite) 
	fprintf(stderr,"  Infinite region: %3d, numberOfInstances: %3d, used: %3d, waste: %3d, noObj: %3d, Infinite: %3d.\n",
	       newRegion->regionId, newRegion->numberOfInstances, newRegion->used, newRegion->waste,
	       newRegion->noObj,newRegion->infinite);
      else
	fprintf(stderr,"  Finite region: %3d, numberOfInstances: %3d, used: %3d, waste: %3d, noObj: %3d, Infinite: %3d.\n",
	       newRegion->regionId, newRegion->numberOfInstances, newRegion->used, newRegion->waste,
	       newRegion->noObj,newRegion->infinite);
      for (newObj=newRegion->fObj;newObj!=NULL;newObj=newObj->nObj) {
	fprintf(stderr,"    Starting new object with allocation point: %3d, size: %3d and numberOfInstances: %3d.\n",
	       newObj->atId, newObj->size, newObj->numberOfInstances);
      }
    }
  }

  return;
}

/*----------------------------------------------------------------*
 * OutputProfile:                                                 *
 * Output word data file with all collected data.                 *
 * Layout of file is as follows:                                  *
 *  maxRegion, maxInstances,                                      *
 *  noOfTicks,                                                    *
 *      noOfRegions, stackUse, regionDescUse, cpuTime             *
 *        regionId, noOfInstances, used, waste, noOfObj, infinite *
 *          allocationPoint, size, numberOfInstances              *
 *          |                                                     *
 *	    allocationPoint, size, numberOfInstances              *
 *        |                                                       *
 *        regionId, noOfInstances, used, waste, noOfObj, infinite *
 *          allocationPoint, size, numberOfInstances              *
 *          |                                                     *
 *	    allocationPoint, size, numberOfInstances              *
 *      |                                                         *
 *      noOfRegions,                                              *
 *        regionId, noOfInstances, used, waste, noOfObj, infinite *
 *          allocationPoint, size, numberOfInstances              *
 *          |                                                     *
 *	    allocationPoint, size, numberOfInstances              *
 *        |                                                       *
 *        regionId, noOfInstances, used, waste, noOfObj, infinite *
 *          allocationPoint, size, numberOfInstances              *
 *          |                                                     *
 *	    allocationPoint, size, numberOfInstances              *
 *  |                                                             *
 * Here we put the profiling table profTab:                       *
 *  sizeProfTab,                                                  *
 *    NoOfPages, MaxNoOfPages, NoOfInstances,                     *
 *    MaxNoOfInstances, AllocNow, MaxAlloc                        *
 *  |                                                             *
 *----------------------------------------------------------------*/
void outputProfile(void) {
  int noOfTicks, noOfRegions, noOfObjects;
  int i;
  TickList *newTick;
  ObjectList *newObj;
  RegionList *newRegion;

  if (exportProfileDatafile) {

#if VERBOSE_OUTPUT_PROFILE
    fprintf(stderr,"Writing profiling data to file %s -- ENTER\n", logName);
#endif

    if ((logFile = fopen((char *) &logName, "w")) == NULL) {
      fprintf(stderr,"Can not open logfile.\n");
      exit(-1);
    }

    putw(maxAlloc, logFile);
    putw(maxNoOfInstances, logFile);

    noOfTicks = 0;
    for (newTick=firstTick;newTick!=NULL;newTick=newTick->nTick)
      noOfTicks++;
    putw(noOfTicks, logFile);

    for (newTick=firstTick;newTick!=NULL;newTick=newTick->nTick) {
      noOfRegions = 0;
      for (newRegion=newTick->fRegion;newRegion!=NULL;newRegion=newRegion->nRegion)
	noOfRegions++;
      putw(noOfRegions, logFile);
      putw(newTick->stackUse, logFile);
      putw(newTick->regionDescUse, logFile);
      putw(newTick->time, logFile);

      for (newRegion=newTick->fRegion;newRegion!=NULL;newRegion=newRegion->nRegion) {
	putw(newRegion->regionId, logFile);
	putw(newRegion->numberOfInstances, logFile);
	putw(newRegion->used, logFile);
	putw(newRegion->waste, logFile);
	putw(newRegion->noObj, logFile);
	putw(newRegion->infinite, logFile);

	for (newObj=newRegion->fObj;newObj!=NULL;newObj=newObj->nObj) {
	  putw(newObj->atId, logFile);
	  putw(newObj->size, logFile);
	  putw(newObj->numberOfInstances, logFile);
	}
      }
    }

    /* Output profTab to log file. */
    putw(MAX_REGIONS_TO_PROFILE, logFile);
    for (i=0;i<MAX_REGIONS_TO_PROFILE;i++) {
      putw(profTab[i][NoOfPages], logFile);
      putw(profTab[i][MaxNoOfPages], logFile);
      putw(profTab[i][NoOfInstances], logFile);
      putw(profTab[i][MaxNoOfInstances], logFile);
      putw(profTab[i][AllocNow], logFile);
      putw(profTab[i][MaxAlloc], logFile);
    }

    fclose(logFile);

#if VERBOSE_OUTPUT_PROFILE
    fprintf(stderr,"Writing profiling data to file %s -- LEAVE\n", logName);
#endif
  }
  return;
}

/*----------------------------------------------------------------*
 * printUsage:                                                    *
 *   Print help on screen.                                        *
 *----------------------------------------------------------------*/ 
void printUsage(void) {
  fprintf(stderr,"You have compiled a ML source program under the ML Kit with profiling enabled.\n");
  fprintf(stderr,"This is the target program (%s) which will generate a profile datafile.\n", prgName);
  fprintf(stderr,"The help screen explains how you can set the profiling strategy.\n\n");
  fprintf(stderr,"usage: %s [-notimer n | -realtime | -virtualtime | -profiletime] \n", prgName);
  fprintf(stderr,"      [-microsec n | -sec n] \n");
  fprintf(stderr,"      [-file outFileName] [-noDatafile] [-noStat]\n");
  fprintf(stderr,"      [-profTab] [-verbose] [-help]\n\n");
  fprintf(stderr,"where -notimer n        Profile every n'th function call.\n");
  fprintf(stderr,"      -realtime         Profile with the real timer.\n");
  fprintf(stderr,"      -virtualtime      Profile with the virtual timer.\n");
  fprintf(stderr,"      -profiletime      Profile with the profile timer.\n\n");
  fprintf(stderr,"      -microsec n       If a timer is chosen, then profile every n'th microseconds.\n");
  fprintf(stderr,"      -sec n            If a timer is chosen, then profile every n'th seconds.\n\n");
  fprintf(stderr,"      -file outFileName Use outFileName as profile datafile. Default is %s\n\n", logName);
  fprintf(stderr,"      -profTab          Print profiling table.\n");
  fprintf(stderr,"      -verbose          Verbose mode.\n");
  fprintf(stderr,"      -noDatafile       The profile datafile is not exported. Default is to export the datafile.\n");
  fprintf(stderr,"      -noStat           No statistics are shown after execution.\n");
  fprintf(stderr,"      -help             This help screen.\n");
  exit(0);
}

/*----------------------------------------------------------------*
 * checkArgs:                                                     *
 *   Check arguments given to the program.                        *
 *----------------------------------------------------------------*/
void checkArgs(int argc, char *argv[]) {
  int match;
  strcpy(prgName, (char *)argv[0]);

  while (--argc > 0) {
    ++argv;    /* next parameter. */
    match = 0; /* no match so far. */

    if (strcmp((char *)argv[0],"-notimer")==0) {
      profType = noTimer;
      match = 1;
      if (--argc > 0 && (*++argv)[0]) { /* Is there a number. */
	if ((profNo = atoi((char *)argv[0])) == 0) {
	  fprintf(stderr,"Something wrong with the number no in switch -notimer no.\n");
	  printUsage();
	}
      } else {
	fprintf(stderr,"No number after the switch -notimer.\n");
	printUsage();
      }
    }

    if (strcmp((char *)argv[0],"-realtime")==0) {
      profType = ITIMER_REAL;
      signalType = SIGALRM;
      match = 1;
    }

    if (strcmp((char *)argv[0],"-virtualtime")==0) {
      profType = ITIMER_VIRTUAL;
      signalType = SIGVTALRM;
      match = 1;
    }

    if (strcmp((char *)argv[0],"-profiletime")==0) {
      profType = ITIMER_PROF;
      signalType = SIGPROF;
      match = 1;
    }

    if (strcmp((char *)argv[0],"-profTab")==0) {
      printProfileTab = 1;
      match = 1;
    }

    if (strcmp((char *)argv[0],"-microsec")==0) {
      match = 1;
      if (--argc > 0 && (*++argv)[0]) { /* Is there a number. */
	if ((microsec = atoi((char *)argv[0])) == 0) {
	  fprintf(stderr,"Something wrong with the number no in switch -microsec no.\n");
	  printUsage();
	}
	sec = 0;
      } else {
	fprintf(stderr,"No number after the switch -microsec.\n");
	printUsage();
      }
    }

    if (strcmp((char *)argv[0],"-sec")==0) {
      match = 1;
      if (--argc > 0 && (*++argv)[0]) { /* Is there a number. */
	if ((sec = atoi((char *)argv[0])) == 0) {
	  fprintf(stderr,"Something wrong with the number no in switch -sec no.\n");
	  printUsage();
	}
	microsec = 0;
      } else {
	fprintf(stderr,"No number after the switch -sec.\n");
	printUsage();
      }
    }

    if (strcmp((char *)argv[0],"-file")==0) {
      match = 1;
      if ((argc-1)>0 && (*(argv+1))[0] != '-') {
	--argc;
	++argv;
	strcpy(logName, (char *)argv[0]);     
      } else {
	fprintf(stderr,"No filename after the -file switch.\n");
	printUsage();
      }
      fprintf(stderr,"Using output file %s.\n", logName);
    } 

    if ((strcmp((char *)argv[0], "-h")==0) ||
	(strcmp((char *)argv[0], "-help")==0)) {
      fprintf(stderr,"Help\n");
      match = 1;
      printUsage();
    }

    if ((strcmp((char *)argv[0], "-v")==0) ||
	(strcmp((char *)argv[0], "-verbose")==0)) {
      match = 1;
      verboseProfileTick = 1;
    }

    if ((strcmp((char *)argv[0], "-noDatafile")==0) ||
	(strcmp((char *)argv[0], "-noProfileDatafile")==0)) {
      match = 1;
      exportProfileDatafile = 0;
    }

    if ((strcmp((char *)argv[0], "-noStat")==0) ||
	(strcmp((char *)argv[0], "-noStatistics")==0)) {
      match = 1;
      showStat = 0;
    }

    if (match == 0) {
      fprintf(stderr,"Something wrong with the switches, maybe an unknown switch...\n");
      printUsage();
    }
  }

  return;
}

#endif /*PROFILING*/
