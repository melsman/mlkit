/* stats-data.h
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 */

#ifndef _STATS_DATA_
#define _STATS_DATA_

#ifndef _ML_BASE_
typedef unsigned int Unsigned32_t;
#endif

#include "ml-timer.h"
#include "cntr.h"

typedef struct {
    Time_t		startTime;	/* the time of initialization */
    Unsigned32_t	mask;		/* bitmask, telling which things were */
					/* measured */
    Unsigned32_t	isNewRuntime;	/* true, if this is the new runtime */
					/* new runtime parameters */
    Unsigned32_t	allocSzB;	  /* the size of the allocation space */
    Unsigned32_t	numGens;	  /* the number of generations. */
					/* old runtime parameters */
    Unsigned32_t	softmax;
    Unsigned32_t	ratio;
    Unsigned32_t	pad[8];		/* pad to 64 bytes */
} stat_hdr_t;

typedef struct {
    cntr_t		allocCnt;	/* allocation count (in bytes) */
    Unsigned32_t	numGens;	/* the number of generations collected */
    Time_t		startTime;	
    Time_t		stopTime;	
    Unsigned32_t	pad[9];		/* pad to 64 bytes */
} stat_rec_t;

/* mask bits in header */
#define STATMASK_ALLOC	0x01
#define STATMASK_NGENS	0x02
#define STATMASK_START	0x04
#define STATMASK_STOP	0x08

#ifdef COLLECT_STATS

#define STATS_BUF_SZ	(2048/sizeof(stat_rec_t))

extern bool_t	    StatsOn;	/* if TRUE, then generate stats */
extern int	    StatsFD;	/* the file descriptor to write the data to */

extern stat_rec_t   StatsBuf[];	/* buffer of data */
extern int	    NStatsRecs;	/* the number of records in the buffer */

/* flush out any records in the buffer */
#define STATS_FLUSH_BUF()	{						\
	if (NStatsRecs >= 0) {							\
	    write (StatsFD, (char *)StatsBuf, NStatsRecs*sizeof(stat_rec_t));	\
	    NStatsRecs = 0;							\
	}									\
    }

#define STATS_FINISH()	{							\
	if (++NStatsRecs >= STATS_BUF_SZ) {					\
	    write (StatsFD, (char *)StatsBuf, STATS_BUF_SZ*sizeof(stat_rec_t));	\
	    NStatsRecs = 0;							\
	}									\
    }

#endif

#endif /* !_STATS_DATA_ */

