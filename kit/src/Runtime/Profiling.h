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
  int atId;                /* Allocation point identifier. */
  int size;                /* Size of object in bytes. */
  struct objectList *nObj; /* Pointer to next object. */
} ObjectList;

typedef struct regionList {
  int regionId;                /* id of region. */
  int used;                    /* number of used words in the region.         */
  int waste;                   /* number of not used words in the region.     */
  int noObj;                   /* number of objects with different program points. */
  int infinite;                /* is region finite of infinite. */
  ObjectList *fObj;            /* Pointer to first object. */
  struct regionList * nRegion; /* Pointer to next region. */
} RegionList;

typedef struct tickList {
  RegionList * fRegion;    /* Pointer to first region. */
  int stackUse;            /* Number of words used on the stack excl. regions. */
  int regionDescUse;       /* Number of words used to infinite regiondescriptors on the stack. */
  unsigned int time;       /* Number of 1/CLOCKS_PER_SEC seconds after start (excl. profiling.) */
  struct tickList * nTick; /* Pointer to data for the next tick. */
} TickList;


/* --------------------------------------------------
 * The following two type definitions are for 
 * holding objects for internal fast lookup 
 * during a profile tick; see function profileTick().
 * -------------------------------------------------- */

typedef struct regionListHashList {
  int regionId;
  struct regionList * rl;              /* entry */
  struct regionListHashList * next;    /* next hashed element */
} RegionListHashList;

typedef struct objectListHashList {
  int atId;
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
  int regionId;
  int noOfPages;
  int maxNoOfPages;
  int allocNow;
  int maxAlloc;
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
void profileTick(int *stackTop);
void profiling_on(void);
void profiling_off(void);
void AlarmHandler();
//void Statistik();
void resetProfiler();
void queueMarkProf();  /* tell the time next time there is a profile tick */
char *allocMemProfiling_xx(int i);
ProfTabList* profTabListInsertAndInitialize(ProfTabList* p, int regionId);
void outputProfilePre(void);
void outputProfileTick(TickList *tick);
void outputProfilePost(void);
void calcAllocInGen(Gen *gen, int *alloc, int *allocProf);

void Statistics(void);

extern int noTimer;
extern int profType;
extern int profNo;
extern int signalType;
extern int printProfileTab;
extern int microsec;
extern int sec;
extern char logName[100];
extern int verboseProfileTick;
extern int exportProfileDatafile;
extern int showStat;
extern int doing_prof;
extern int raised_exn_interupt_prof;
extern int raised_exn_overflow_prof;


#else /*PROFILING not defined */

void queueMark();  /* does nothing */

#endif /*PROFILING*/

#endif /*PROFILING_H*/
