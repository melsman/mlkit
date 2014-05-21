/*----------------------------------------------------------------*
 *                         Profiling                              *
 *----------------------------------------------------------------*/

#ifndef PROFILING_H
#define PROFILING_H

#ifdef PROFILING
#include "Flags.h"
#include "Region.h"

#define BYTES_ALLOC_TO_PROFILING 1024

typedef struct objectList {
  long atId;                /* Allocation point identifier. */
  long size;                /* Size of object in bytes. */
  struct objectList *nObj; /* Pointer to next object. */
} ObjectList;

typedef struct regionList {
  long regionId;                /* id of region. */
  long used;                    /* number of used words in the region.         */
  long waste;                   /* number of not used words in the region.     */
  long noObj;                   /* number of objects with different program points. */
  long infinite;                /* is region finite of infinite. */
  ObjectList *fObj;            /* Pointer to first object. */
  struct regionList * nRegion; /* Pointer to next region. */
} RegionList;

typedef struct tickList {
  RegionList * fRegion;    /* Pointer to first region. */
  long stackUse;            /* Number of words used on the stack excl. regions. */
  long regionDescUse;       /* Number of words used to infinite regiondescriptors on the stack. */
  unsigned long time;       /* Number of 1/CLOCKS_PER_SEC seconds after start (excl. profiling.) */
  struct tickList * nTick; /* Pointer to data for the next tick. */
} TickList;


/* --------------------------------------------------
 * The following two type definitions are for 
 * holding objects for internal fast lookup 
 * during a profile tick; see function profileTick().
 * -------------------------------------------------- */

typedef struct regionListHashList {
  long regionId;
  struct regionList * rl;              /* entry */
  struct regionListHashList * next;    /* next hashed element */
} RegionListHashList;

typedef struct objectListHashList {
  long atId;
  struct objectList * ol;              /* entry */
  struct objectListHashList * next;    /* next hashed element */  
} ObjectListHashList;

#define REGION_LIST_HASH_TABLE_SIZE 4096
#define OBJECT_LIST_HASH_TABLE_SIZE 4096

#define regionListHashTabIndex(regionId)  ((regionId) & (REGION_LIST_HASH_TABLE_SIZE-1))
#define objectListHashTabIndex(atId)      ((atId) & (OBJECT_LIST_HASH_TABLE_SIZE-1))



/* ---------------------------------------------------
 * A global profiling table is used to collect
 * information during execution (independently of
 * profiling ticks). The information is stored in
 * a map with regionIds as domain; the table is
 * hashed to make its size independent of the actual
 * regionIds.
 * --------------------------------------------------- */

typedef struct profTabList {
  long regionId;
  long noOfPages;
  long maxNoOfPages;
  long allocNow;
  long maxAlloc;
  struct profTabList * next;
} ProfTabList;

/* size of hash table */

/*
#define PROF_HASH_TABLE_SIZE 3881
#define profHashTabIndex(regionId) ((regionId) % PROF_HASH_TABLE_SIZE)
*/

#define PROF_HASH_TABLE_SIZE 4096
#define profHashTabIndex(regionId) ((regionId) & (PROF_HASH_TABLE_SIZE-1))

extern ProfTabList * profHashTab[];

/*----------------------------------------------------------------*
 *        Prototypes for external and internal functions.         *
 *----------------------------------------------------------------*/

void checkProfTab(char* s);
void printProfTab(void);
void profileTick(long *stackTop);
void profiling_on(void);
void profiling_off(void);
void AlarmHandler();
//void Statistik();
void resetProfiler();
void queueMarkProf();  /* tell the time next time there is a profile tick */
char *allocMemProfiling_xx(long i);
ProfTabList* profTabListInsertAndInitialize(ProfTabList* p, long regionId);
void outputProfilePre(void);
void outputProfileTick(TickList *tick);
void outputProfilePost(void);
void calcAllocInGen(Gen *gen, long *alloc, long *allocProf);

void profTabIncrNoOfPages(long regionId, long i);
void profTabMaybeIncrMaxNoOfPages(long regionId);
void profTabDecrNoOfPages(long regionId, long i);
void profTabDecrAllocNow(long regionId, long i, char *s);
void profTabIncrAllocNow(long regionId, long i);

void Statistics(void);

extern long noTimer;
extern long profType;
extern long profNo;
extern long signalType;
extern long printProfileTab;
extern long microsec;
extern long sec;
extern char logName[100];
extern long verboseProfileTick;
extern long exportProfileDatafile;
extern long showStat;
extern long doing_prof;
extern long raised_exn_interupt_prof;
extern long raised_exn_overflow_prof;


#else /*PROFILING not defined */

void queueMark();  /* does nothing */

#endif /*PROFILING*/

#endif /*PROFILING_H*/
