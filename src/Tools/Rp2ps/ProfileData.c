/* This module contains functions for manipolating */
/* profiling data.                                 */
#include <stdio.h>
#include <time.h>
#include "Flags.h"
#include "Error.h"
#include "Alloc.h"
#include "ProfileData.h"
#include "Graph.h"
#include "Sample.h"
#include "Rp2Ps.h"

/* SUN_OS4 reports the time passed from clock in microsecs. */
/* This is used in file Profiling.c in the runtimesystem.   */
#ifdef SUN_OS4
#define CLOCKS_PER_SEC 1000000
#endif /*SUN_OS4*/

/***************************************************************
 * Declarations used to construct the graph data structure.    *
 ***************************************************************/

TickList * firstTick; /* Pointer to data for the first tick. */
TickList * lastTick;  /* Pointer to data for the last tick. */

int maxRegions = 0;         /* max. number of allocated words in regions. */
int maxInstances = 0;       /* max. number of instances.       */

ProfTabList * profHashTab[profHashTabSize];  /* Hash table for profiling */

void initializeProfTab(void) {
  int i;
  for (i=0;i<profHashTabSize;i++) 
    profHashTab[i]=NULL;
  return;
}

ProfTabList* profTabListInsertAndInitialize(ProfTabList* p, int regionId) {
  ProfTabList* pNew;
  pNew = (ProfTabList*)malloc(sizeof(ProfTabList));
  if (pNew == (ProfTabList*) -1) {
    printf("profTabListInsertAndInitialize error\n");
    exit(-1);
  }
  pNew->regionId=regionId;
  pNew->maxAlloc=0;
  pNew->next=p;
  return pNew;
}

int profTabGetMaxAlloc(int regionId) {
  ProfTabList* p;
  for (p=profHashTab[regionId % profHashTabSize]; p != NULL; p=p->next)
    if (p->regionId == regionId) return p->maxAlloc;
  return 0;
}

void profTabSetMaxAlloc(int regionId, int no) {
  ProfTabList* p;
  int index;
  index = regionId % profHashTabSize;
  for (p=profHashTab[index]; p != NULL; p=p->next)
    if (p->regionId == regionId) {
      p->maxAlloc = no;
      return;
    };
  p = profTabListInsertAndInitialize(profHashTab[index], regionId);
  profHashTab[index] = p;
  p->maxAlloc = no;
  return;
}


static unsigned int max(unsigned int a, unsigned int b) {
  return (a<b)?b:a;
}

static unsigned int min(unsigned int a, unsigned int b) {
  return (a<b)?a:b;
}

/*----------------------------------------------------------------*
 * Input word data file with all collected data.                  *
 * Layout of file is as follows:                                  *
 *  maxRegion, maxInstances,                                      *
 *  noOfTicks,                                                    *
 *      noOfRegions, stackUse, regionDescUse, time                *
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
 *    regionId, MaxAlloc                                          *
 *  |                                                             *
 *----------------------------------------------------------------*/
void inputProfile(void) {
  int noOfTicks, noOfRegions, noOfObjects;
  int i, regionId, w;
  TickList *newTick;
  ObjectList *newObj;
  RegionList *newRegion;

  if ((logFile = fopen((char *) &logName, "r")) == NULL) {
    printf("Cannot open input file %s.\n", logName);
    exit(-1);
  }

  readWord(maxRegions, logFile);
  readWord(maxInstances, logFile); 

  readWord(noOfTicks, logFile);
  noOfSamples = noOfTicks;
  while (noOfTicks--) {

    /* Allocate new tick. */
    newTick = (TickList *)malloc(sizeof(TickList));
    newTick->nTick   = NULL;
    newTick->fRegion = NULL;
    if (firstTick == NULL)
      firstTick = newTick;
    else
      lastTick->nTick = newTick;
    lastTick = newTick;

    readWord(noOfRegions, logFile);
    readWord(newTick->stackUse, logFile);
    readWord(newTick->regionDescUse, logFile);
    readWord(newTick->time, logFile);

    /* Get all regions in the tick. */
    while (noOfRegions--) {
      /* Allocate new region. */
      newRegion = (RegionList *)malloc(sizeof(RegionList));
      readWord(newRegion->regionId, logFile);
      readWord(newRegion->numberOfInstances, logFile);
      readWord(newRegion->used,  logFile);
      readWord(newRegion->waste, logFile);
      readWord(newRegion->noObj, logFile);
      readWord(newRegion->infinite, logFile);

      newRegion->nRegion = newTick->fRegion;
      newTick->fRegion = newRegion;
      newRegion->fObj = NULL;

      /* Get all objects in the tick. */
      noOfObjects = newRegion->noObj;
      while (noOfObjects--) {
	/* Allocate new object. */
	newObj = (ObjectList *)malloc(sizeof(ObjectList));
	readWord(newObj->atId, logFile);
	readWord(newObj->size, logFile);
	readWord(newObj->numberOfInstances, logFile);
	newObj->nObj = newRegion->fObj;
	newRegion->fObj = newObj;
      }
    }
  }

  /* Input profTab from log file. */
  initializeProfTab();
  readWord(i, logFile);
  for ( ; i > 0 ; i-- ) {
    readWord(regionId,logFile);
    readWord(w,logFile);
    profTabSetMaxAlloc(regionId,w);
  }

  fclose(logFile);

  return;
}

/* Sorts out no samples in the range [min, ..., max].     */
/* min and max have to be in range [1, ..., noOfSamples], */
/* and sampleNoTab have range [0, ..., noOfSamples-1].    */
static void sortOutBin(int min, int max, int no, int *sampleNoTab)
{
  int mid, no1, no2;

  mid = (min+max)/2;
  if (no == 1) {
    if (sampleNoTab[mid-1] == -1)
      Disaster("sortOutBin: sampleNoTab already -1.");
    sampleNoTab[mid-1] = -1;
  }
  else
    if (no > 1) {
      no1 = no / 2;
      no2 = no-no1;
      sortOutBin(min, mid, no2, sampleNoTab);
      sortOutBin(mid+1, max, no1, sampleNoTab);
    }
  return;
}

/* Returns number of samples in sampleNoTab. */
static int sortSamples(int sortType, int *sampleNoTab) 
{
  int i, j, m;                     /* Counters */
  int *sampleSizeTab ;             /* Table holding size of each sample. */
  TickList *newTick;               /* Used to walk through the samples.  */
  RegionList *newRegion;           /* Used to walk through the regions in each sample. */
  int total, temp;                 /* Temporary variables. */
  int sampleNo;                    

  if (sortType == TAKE_BY_SAMPLE_NO) {

    /* We keep min(MaxSamples, noOfSamples) samples in sampleNoTab sorted    */
    /* by sampleNo, and equally distributed in the range of sample numbers.  */
    /* If noOfSamples <= SampleMax then all samples are returned.            */
    /* If noOfSamples > SampleMax, then sortOutBin is used to sort out       */
    /* noOfSamples-SampleMax samples by divide and conquer.                  */
    for (i=0;i<noOfSamples;i++)
      sampleNoTab[i] = i;
    if (noOfSamples <= SampleMax)
      /* Return all samples. */
      return noOfSamples; 
    else { 
      /* After sortOutBin all samples which have to be sorted out have the */
      /* value -1.                                                         */
      sortOutBin(1, noOfSamples, noOfSamples-SampleMax, sampleNoTab);
      
      i = 0;
      sampleNo=0;
      for (i=0;i<noOfSamples;i++) {
        #if DEBUG_SORT_SAMPLES
	  printf("SortSamples (SORT_BY_SAMPLE_NO): sampleNoTab[%3d] = %3d\n", i, sampleNoTab[i]);
        #endif
	if (sampleNoTab[i] >= 0) {
	  sampleNoTab[sampleNo] = sampleNoTab[i];
	  sampleNo++;
	}
      }
      if (sampleNo != SampleMax)  /* These two variables have to be equal. */
	Disaster("SortSamples: SampleMax <> sampleNo");
    
      return sampleNo;
    }
  } else { /* sortType == TAKE_BY_SIZE. */
    /* We keep the SampleMax largest samples. */

    /* First make a sampleSizeTab holding the size of each sample, and */
    /* a sampleNoTab holding the sampleNo w.r.t. the sampleSizeTab.    */
    sampleSizeTab = xmalloc(noOfSamples * sizeof(int));
    i=0;
    for (newTick=firstTick;newTick!=NULL;newTick=newTick->nTick) {
      total = 0;
      for (newRegion=newTick->fRegion;newRegion!=NULL;newRegion=newRegion->nRegion)
	total += newRegion->used;
      sampleSizeTab[i] = total;
      sampleNoTab[i] = i;
      i++;
    }

    /* Then sort the two tables w.r.t. the size of each sample. */
    for (i=0; i<noOfSamples-1;i++) {
      m = i;
      for (j=i+1; j<noOfSamples;j++) {
	if (sampleSizeTab[j] > sampleSizeTab[m])
	  m = j;
      }
      temp = sampleSizeTab[m];
      sampleSizeTab[m] = sampleSizeTab[i];
      sampleSizeTab[i]=temp;

      temp = sampleNoTab[m];
      sampleNoTab[m]=sampleNoTab[i];
      sampleNoTab[i]=temp;
    }

    for (i=0; i<(min(SampleMax,noOfSamples)-1);i++) {
      m = i;
      for (j=i+1; j<min(SampleMax,noOfSamples);j++) {
	if (sampleNoTab[j] < sampleNoTab[m])
	  m = j;
      }
      temp = sampleSizeTab[m];
      sampleSizeTab[m] = sampleSizeTab[i];
      sampleSizeTab[i]=temp;

      temp = sampleNoTab[m];
      sampleNoTab[m]=sampleNoTab[i];
      sampleNoTab[i]=temp;
    }
    return min(SampleMax, noOfSamples);
  }
} 

/*----------------------------------------------------------------*
 * PrintProfile:                                                  *
 *   Print all collected data on screen.                          *
 *----------------------------------------------------------------*/
void PrintProfile(void) {
  TickList *newTick;
  ObjectList *newObj;
  RegionList *newRegion;

  for (newTick=firstTick;newTick!=NULL;newTick=newTick->nTick) {
    printf("Starting new tick with stackUse: %5d and regionDescUse: %5d.\n", newTick->stackUse, newTick->regionDescUse);
    for (newRegion=newTick->fRegion;newRegion!=NULL;newRegion=newRegion->nRegion) {
      if (newRegion->infinite) 
	printf("  Infinite region: %3d, numberOfInstances: %3d, used: %3d, waste: %3d, noObj: %3d, Infinite: %3d.\n",
	       newRegion->regionId, newRegion->numberOfInstances, newRegion->used, newRegion->waste,
	       newRegion->noObj,newRegion->infinite);
      else
	printf("  Finite region: %3d, numberOfInstances: %3d, used: %3d, waste: %3d, noObj: %3d, Infinite: %3d.\n",
	       newRegion->regionId, newRegion->numberOfInstances, newRegion->used, newRegion->waste,
	       newRegion->noObj,newRegion->infinite);
      for (newObj=newRegion->fObj;newObj!=NULL;newObj=newObj->nObj) {
	printf("    Starting new object with allocation point: %3d, size: %3d and numberOfInstances: %3d.\n",
	       newObj->atId, newObj->size, newObj->numberOfInstances);
      }
    }
  }

  return;
}

/*----------------------------------------------------------------*
 * PrintRegion:                                                   *
 *   Print statistics on objects in a region.                     *
 *----------------------------------------------------------------*/
void PrintRegion(int region) {
  int i;
  TickList *newTick;
  ObjectList *newObj;
  RegionList *newRegion;

  printf("Print object profiling on region %d\n", region);
  i = 0;
  for (newTick=firstTick;newTick!=NULL;newTick=newTick->nTick) {
    printf("Tick number %d\n", i);
    for (newRegion=newTick->fRegion;newRegion!=NULL;newRegion=newRegion->nRegion)
      if (newRegion->regionId == region) {
	printf("Region data:\n");
	if (newRegion->infinite)
	  printf("  Infinite region id: %5d\n", newRegion->regionId);
	else
	  printf("  Finite region id:   %5d\n", newRegion->regionId);
	printf("  Used:                 %5d\n", newRegion->used);
	printf("  Waste:                %5d\n", newRegion->waste);
	printf("  Instances:            %5d\n", newRegion->numberOfInstances);
	for (newObj=newRegion->fObj;newObj!=NULL;newObj=newObj->nObj) 
	  printf("  Object %4d with size %5d words and %5d instances. Each object %5.2f.\n", 
                 newObj->atId, newObj->size, newObj->numberOfInstances,
		 newObj->size/(newObj->numberOfInstances+0.0));
      }
    i++;
  }
  
  return;
}

/*----------------------------------------------------------------*
 * PrintSomeStat:                                                 *
 *   Print some stat of the collected profiling data.             *
 *----------------------------------------------------------------*/
void PrintSomeStat(void) {
  int i, noOfTicks=0;
  int minRd=10000, maxRd=0, minPP=10000, maxPP=0;
  int totalUsedI=0, totalWasteI=0, totalUsedF=0, totalWasteF=0; 
  TickList *newTick;
  ObjectList *newObj;
  RegionList *newRegion;

  printf(" Some statistics on the read data.\n\n");
  i = 0;
  for (newTick=firstTick;newTick!=NULL;newTick=newTick->nTick) {
    totalUsedI = 0;
    totalUsedF = 0;
    totalWasteI = 0;
    totalWasteF = 0;
    noOfTicks++;
    for (newRegion=newTick->fRegion;newRegion!=NULL;newRegion=newRegion->nRegion) {
      if (newRegion->infinite) {
	totalUsedI += newRegion->used;
	totalWasteI += newRegion->waste;
      }
      else {
	totalUsedF += newRegion->used;
	totalWasteF += newRegion->waste;
      }

      if (newRegion->regionId < minRd)
	minRd = newRegion->regionId;
      if (newRegion->regionId > maxRd)
	maxRd = newRegion->regionId;
      for (newObj=newRegion->fObj;newObj!=NULL;newObj=newObj->nObj) {
	if (newObj->atId < minPP && newObj->atId > 0)
	  minPP = newObj->atId;
	if (newObj->atId > maxPP)
	  maxPP = newObj->atId;
      }
    }
    printf("  Tick number %d at time %d\n", noOfTicks, newTick->time);
    printf("  Total used space in tick %d in words: Finite %d and Infinite %d.\n", i, totalUsedF, totalUsedI);
    printf("  Total waste in tick %d in words: Finite %d and Infinite %d.\n", i, totalWasteF, totalWasteI);
    printf("  Stack use: %5d and infinite region desc use: %5d\n", newTick->stackUse, newTick->regionDescUse);
    i++;
  }

  printf("  Number of ticks: %d\n", noOfTicks);

  printf("  Regions are in the interval %d ... %d\n",minRd, maxRd);
  printf("  Program points (excl. 0) are in the interval %d ... %d\n",minPP, maxPP);

  return;
}

/* Makes a region profile. */
void MakeRegionProfile(void) {
  int i, sampleNo, no;
  TickList *newTick;
  RegionList *newRegion;
  int *sampleNoTab;
  float sampleTime;
  float maxStack = 0.0;
  
  char idStr[100];

  GraphReset();

  if ((outfp = fopen((char *) &rpName, "w")) == NULL) {
    printf("Can not open output file %s.\n", rpName);
    exit(-1);
  }

  sprintf(idStr, "%s - Region profiling", name);
  jobstring = MallocString(idStr);
  sprintf(idStr, "%s", timeStr);
  datestring = MallocString(idStr);

  sampleNoTab = xmalloc(noOfSamples * sizeof(int));

  no = sortSamples(sortOpt, sampleNoTab);
  i = 0;
  sampleNo = 0;
  for (newTick=firstTick;newTick!=NULL;newTick=newTick->nTick,i++) {
    if ((sampleNo < no) && (sampleNoTab[sampleNo] == i)) {
      if (useTickNo) {
	sampleTime = (float)i;
      }
      else {
	sampleTime = (float)newTick->time/(float)CLOCKS_PER_SEC;
      }
      /*printf("sampleNo %3d SampleTime %5.2f\n", sampleNo, sampleTime);*/
      allocNewSample(sampleNo, sampleTime);
      storeSampleEntry(sampleNo, sampleTime, "stack", ((float)(newTick->stackUse))*4.0);
      storeSampleEntry(sampleNo, sampleTime, "rDesc", ((float)(newTick->regionDescUse))*4.0);

      if (((((float)(newTick->stackUse))+((float)(newTick->regionDescUse)))*4.0) > maxStack) /* To ajust the max. */
	maxStack = (((float)(newTick->stackUse))+((float)(newTick->regionDescUse)))*4.0;     /* allocation line.  */
	  
      for (newRegion=newTick->fRegion;newRegion!=NULL;newRegion=newRegion->nRegion) {
	if (newRegion->infinite) 
	  sprintf(idStr, "r%dinf", newRegion->regionId);
	else 
	  sprintf(idStr, "r%dfin", newRegion->regionId);

	storeSampleEntry(sampleNo, sampleTime, idStr, ((float)(newRegion->used))*4.0);
      }
      sampleNo++;
    }
  }

  if (sampleNo != nsamples)           /* These two variables have to follow each other. */
    Disaster("sampleNo <> nsamples");
    
  if ((noOfSamples >= SampleMax) && (SampleMax != nsamples))         /* If we have more than SampleMax samples, */
    Disaster("noOfSamples >= SampleMax and SampleMax <> nsamples."); /* then we keep exactly SampleMax samples. */

  showMax = 1;
  maxValue = maxRegions*4;
  sprintf(maxValueStr, "Maximum allocated bytes in regions: %2.0f.", maxValue);
  maxValue += maxStack; /* Ajusting the max. allocation line. */
  yLab = MallocString("bytes");
  PutFile();
  return;
}

/* Makes a stack profile on file stackFile. */
void MakeStackProfile(void) {
  int i, no, sampleNo;
  TickList *newTick;
  int *sampleNoTab;
  float sampleTime;
  
  char idStr[100];

  GraphReset();

  if ((outfp = fopen((char *) &stackName, "w")) == NULL) {
    printf("Can not open stackfile: %s.\n", stackName);
    exit(-1);
  }

  sprintf(idStr, "%s - Stack profiling", name);
  jobstring = MallocString(idStr);
  sprintf(idStr, "%s", timeStr);
  datestring = MallocString(idStr);

  sampleNoTab = xmalloc(noOfSamples * sizeof(int));

  no = sortSamples(sortOpt, sampleNoTab);
  i = 0;
  sampleNo = 0;
  for (newTick=firstTick;newTick!=NULL;newTick=newTick->nTick,i++) {
    if ((sampleNo < no) && (sampleNoTab[sampleNo] == i)) {
      if (useTickNo) {
	sampleTime = (float)i;
      }
      else {
	sampleTime = (float)newTick->time/(float)CLOCKS_PER_SEC;
      }
      /*printf("sampleNo %3d SampleTime %5.2f\n", sampleNo, sampleTime);*/
      allocNewSample(sampleNo, sampleTime);
      storeSampleEntry(sampleNo, sampleTime, "stack", ((float)(newTick->stackUse))*4.0);
      storeSampleEntry(sampleNo, sampleTime, "rDesc", ((float)(newTick->regionDescUse))*4.0);

      sampleNo++;
    }
  }

  if (sampleNo != nsamples)           /* These two variables have to follow each other. */
    Disaster("sampleNo <> nsamples");
    
  if ((noOfSamples >= SampleMax) && (SampleMax != nsamples))         /* If we have more than SampleMax samples, */
    Disaster("noOfSamples >= SampleMax and SampleMax <> nsamples."); /* then we keep exactly SampleMax samples. */

  showMax = 0; /* Do not show a maximum line. */
  yLab = MallocString("bytes");
  PutFile();
  return;
}

/* Makes a region profile. */
void MakeRegionInstanceProfile(void) {
  int i, sampleNo, no;
  TickList *newTick;
  RegionList *newRegion;
  int *sampleNoTab;
  float sampleTime;
  
  char idStr[100];

  GraphReset();

  if ((outfp = fopen((char *) &rpiName, "w")) == NULL) {
    printf("Can not open output file %s.\n", rpiName);
    exit(-1);
  }

  sprintf(idStr, "%s - Region Instance profiling", name);
  jobstring = MallocString(idStr);
  sprintf(idStr, "%s", timeStr);
  datestring = MallocString(idStr);

  sampleNoTab = xmalloc(noOfSamples * sizeof(int));

  no = sortSamples(sortOpt, sampleNoTab);
  i = 0;
  sampleNo = 0;
  for (newTick=firstTick;newTick!=NULL;newTick=newTick->nTick,i++) {
    if ((sampleNo < no) && (sampleNoTab[sampleNo] == i)) {
      if (useTickNo) {
	sampleTime = (float)i;
      }
      else {
	sampleTime = (float)newTick->time/(float)CLOCKS_PER_SEC;
      }
      /*printf("sampleNo %3d SampleTime %5.2f\n", sampleNo, sampleTime);*/
      allocNewSample(sampleNo, sampleTime);
	  
      for (newRegion=newTick->fRegion;newRegion!=NULL;newRegion=newRegion->nRegion) {
	if (newRegion->infinite) 
	  sprintf(idStr, "r%dinf", newRegion->regionId);
	else 
	  sprintf(idStr, "r%dfin", newRegion->regionId);

	storeSampleEntry(sampleNo, sampleTime, idStr, ((float)(newRegion->numberOfInstances)));
      }
      sampleNo++;
    }
  }

  if (sampleNo != nsamples)           /* These two variables have to follow each other. */
    Disaster("sampleNo <> nsamples");
    
  if ((noOfSamples >= SampleMax) && (SampleMax != nsamples))         /* If we have more than SampleMax samples, */
    Disaster("noOfSamples >= SampleMax and SampleMax <> nsamples."); /* then we keep exactly SampleMax samples. */

  showMax = 1;
  maxValue = maxInstances;
  sprintf(maxValueStr, "Maximum number of instances: %2.0f.", maxValue);
  yLab = MallocString("instances");
  PutFile();
  return;
}

/* Makes a region profile. */
void MakeObjectProfile(int region) {
  int i, sampleNo, no;
  TickList *newTick;
  ObjectList *newObj;
  RegionList *newRegion;
  int *sampleNoTab;
  float sampleTime;
  
  char idStr[100];

  GraphReset();

  if ((outfp = fopen((char *) &objName, "w")) == NULL) {
    printf("Can not open output file %s.\n", objName);
    exit(-1);
  }

  sprintf(idStr, "%s - Object profiling on region %d", name, region);
  jobstring = MallocString(idStr);
  sprintf(idStr, "%s", timeStr);
  datestring = MallocString(idStr);

  sampleNoTab = xmalloc(noOfSamples * sizeof(int));

  no = sortSamples(sortOpt, sampleNoTab);
  i = 0;
  sampleNo = 0;
  for (newTick=firstTick;newTick!=NULL;newTick=newTick->nTick,i++) {
    if ((sampleNo < no) && (sampleNoTab[sampleNo] == i)) {
      if (useTickNo) {
	sampleTime = (float)i;
      }
      else {
	sampleTime = (float)newTick->time/(float)CLOCKS_PER_SEC;
      }
      /*printf("sampleNo %3d SampleTime %5.2f\n", sampleNo, sampleTime);*/
      allocNewSample(sampleNo, sampleTime);
	  
      for (newRegion=newTick->fRegion;newRegion!=NULL;newRegion=newRegion->nRegion) {
	if (newRegion->regionId == region) 
	  for (newObj=newRegion->fObj;newObj!=NULL;newObj=newObj->nObj) {
	    sprintf(idStr, "pp%d", newObj->atId);
	    storeSampleEntry(sampleNo, sampleTime, idStr, ((float)(newObj->size))*4.0);
	  }
      }
      sampleNo++;
    }
  }

  if (sampleNo != nsamples)           /* These two variables have to follow each other. */
    Disaster("sampleNo <> nsamples");
    
  if ((noOfSamples >= SampleMax) && (SampleMax != nsamples))         /* If we have more than SampleMax samples, */
    Disaster("noOfSamples >= SampleMax and SampleMax <> nsamples."); /* then we keep exactly SampleMax samples. */

  showMax = 1;
  maxValue = profTabGetMaxAlloc(region)*4;
  sprintf(maxValueStr, "Maximum allocated bytes in this region: %2.0f.", maxValue);
  yLab = MallocString("bytes");
  PutFile();
  return;
}

/* Makes a region profile. */
void FindProgramPoint(int pPoint) {
  TickList *newTick;
  ObjectList *newObj;
  RegionList *newRegion;
  
  for (newTick=firstTick;newTick!=NULL;newTick=newTick->nTick) {
    for (newRegion=newTick->fRegion;newRegion!=NULL;newRegion=newRegion->nRegion) {
      for (newObj=newRegion->fObj;newObj!=NULL;newObj=newObj->nObj) {
	if (newObj->atId == pPoint)
	  printf("Program point %5d is in region %5d\n", pPoint, newRegion->regionId);
      }
    }
  }

  return;
}
