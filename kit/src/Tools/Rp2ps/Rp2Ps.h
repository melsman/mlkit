#ifndef RP_2_PS
#define RP_2_PS

#include <stdio.h>
#include "Flags.h"
#include "Types.h"

/*
       +-----+           addComment/addMark
       |Rp2Ps|------------------------------------+
       +-----+                                    |
       	 |                                        |
	 |MakeRegionProfile                       |
	\|/                                       |
     +-----------+                                |
     |ProfileData|-----------+                    |
     +-----------+           |                    |
	 |                   |storeSampleEntry    |
	 |PutFile            |                    |
        \|/                 \|/                   |
      +-----+           +-----------+             |
      |graph|--------+  |Sample     |<------------+
      +-----+        |  +-----------+
         |           |
         |           +---------+
        \|/TitleText          \|/CurvesInit
      +------+             +------+
      |Output|<------------|Curves|
      +------+  stringSize +------+
         |                     
         |                     
         |outputPS             
        \|/                     
   +----------+                
   |postScript|
   +----------+

*/


/*------------------------------------------------*
 * We have all global declarations in this module *
 *------------------------------------------------*/
extern char* programname; /* Name of program to put on graph. */
extern char* jobstring;   /* Name of job to put on graph.     */
extern char* datestring;  /* Date to put on graph.            */

extern char  logName[];   /* Name of log file to use. */
extern FILE* logFile;
extern char  rpName[];    /* Name of regionProfile file. */
extern char  stackName[]; /* Name of regionProfile file. */
extern char outFileName[];
extern char prgName[];
extern FILE* stackFile;
extern char  objName[];   /* Name of regionProfile file. */
extern FILE* objFile;
extern char  rpiName[];   /* Name of regionProfile file. */
extern FILE* rpiFile;
extern char  dotEnd[];    /* Default end of filenames. */
extern char  name[];      /* Name of regionProfile file. */
extern char  tempStr[];   /* temporary string. */

extern FILE *outfp; /* Output file. */

/*-------------------------------*
 * Command line options.         *
 *-------------------------------*/
extern int yflag;         /* Don't show vertical lines in graph.              */
extern int eflag;         /* Switch used when doing encapsulated post script. */
extern double epsfwidth;  /* Width used when doing encapsulated post script.  */
extern int gflag;
extern int SampleMax;     /* max. number of samples.         */
extern int noOfSamples;   /* Number of samples (same as ticks) in input file. */
extern int useTickNo;     /* Use tick number on x-axis instead of seconds. */
extern int fixedYRange;   /* Use fixed range on y-axis. */
extern int cflag;         /* Is one when printing comments. */
extern int mflag;         /* Is one when printing marks. */

#define TAKE_BY_SIZE 0
#define TAKE_BY_SAMPLE_NO 1
extern int sortOpt;

#define MAX_NO_OF_BANDS 20
extern int noOfBands;      /* Number of bands shown on the graph. */
extern int showMax;        /* Don't show a maximum on the graph. */
extern double maxValue;    /* The maximal value shown if showMax = 1. */
extern char maxValueStr[]; /* String used when printing maxValue. */
extern char  *timeStr;     /* Ptr. to string with current time. */
extern char *yLab;

/*-------------------------------------*
 * Global Sample declarations.         *
 *-------------------------------------*/
extern float* sampletable;     /* sample intervals.               */
extern int    nsamples;        /* number of samples.              */
extern float* marktable;       /* table holding mark times.       */
extern int    nmarks;          /* number of marks.                */
extern int    nmarkmax;        /* max. number of marks so far.    */
extern float* commenttable;    /* table holding comment times.    */
extern char **commentstring;   /* table holding comment strings.  */
extern int    ncomments;       /* number of comments.             */
extern int    ncommentmax;     /* max. number of comments so far. */

#define N_MARKS 20             /* To start with we have N_MARKS.  */
extern ENTRYPTR* identtable;   /* table holding identifier ptr's. */
extern int nidents;            /* number of identifiers.          */


#endif /*RP_2_PS */


