/*----------------------------------------------------------------*
 *                         Profiling                              *
 *----------------------------------------------------------------*/

#ifndef PROF
#define PROF

/*----------------------------------------------------------------*
 * Include files                                                  *
 * Compiling: cc -Aa -c Profiling.c                               *
 *----------------------------------------------------------------*/

#include "Flags.h"

#ifdef PROFILING
/*----------------------------------------------------------------*
 *        Declarations for the profiling tool                     *
 *----------------------------------------------------------------*/

#define BYTES_ALLOC_TO_PROFILING 1024

typedef struct objectList {
  int atId;                /* Allocation point identifier. */
  int size;                /* Size of object in bytes. */
  int numberOfInstances;   /* Number of instances of an object at an allocation point. */
  struct objectList *nObj; /* Pointer to next object. */
} ObjectList;
#define sizeObjectList (sizeof(ObjectList)/4)

typedef struct regionList {
  int regionId;                /* id of region. */
  int numberOfInstances;       /* number of instances of the region(variable) */
  int used;                    /* number of used words in the region.         */
  int waste;                   /* number of not used words in the region.     */
  int noObj;                   /* number of objects with different program points. */
  int infinite;                /* is region finite of infinite. */
  ObjectList *fObj;            /* Pointer to first object. */
  struct regionList * nRegion; /* Pointer to next region. */
} RegionList;
#define sizeRegionList (sizeof(RegionList)/4)

typedef struct tickList {
  RegionList * fRegion;    /* Pointer to first region. */
  int stackUse;            /* Number of words used on the stack excl. regions. */
  int regionDescUse;       /* Number of words used to infinite regiondescriptors on the stack. */
  unsigned int time;       /* Number of 1/CLOCKS_PER_SEC seconds after start (excl. profiling.) */
  struct tickList * nTick; /* Pointer to data for the next tick. */
} TickList;
#define sizeTickList (sizeof(TickList)/4)


/* Entries in hash table for region ids */
typedef struct profTabList {
  int regionId;
  int noOfPages;
  int maxNoOfPages;
  int noOfInstances;
  int maxNoOfInstances;
  int allocNow;
  int maxAlloc;
  struct profTabList * next;
} ProfTabList;

#define profHashTabSize 3881
/*extern ProfTabList * profHashTab[];*/


/*----------------------------------------------------------------*
 *        Prototypes for external and internal functions.         *
 *----------------------------------------------------------------*/

void profileTick(int *stackTop);
void profiling_on(void);
void profiling_off(void);
void AlarmHandler();
void Statistik();
void resetProfiler();
void updateMaxProfStack();
void queueMark();  /* does nothing */
void queueMarkProf();  /* tell the time next time there is a profile tick */

#endif /*PROFILING*/

#endif /*PROF*/
