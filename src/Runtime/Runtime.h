/*----------------------------------------------------------------*
 *             Runtime system for the ML-Kit                      *
 *----------------------------------------------------------------*/

#ifndef RUNTIME
#define RUNTIME

/*----------------------------------------------------------------*
 * Include files                                                  *
 * Compiling: cc -Aa -c Runtime.c                                 *
 *----------------------------------------------------------------*/
#include "String.h"
#include "Flags.h"

/* LOG:

*/


/* Structure of the runtime system is as follows:          */
/*

Function dependencies:


                +-------------------+	            +-------+
                |IO                 |               |Runtime|
                +-------------------+	            +-------+
                | openFile          |	            | main  |	       equalPoly
                | closeFile         |	      +-----|       |-------------------+
                | ...               |	      |     |       |	        	|
                +-------------------+	      |     +-------+	        	|
                        |		KITdie|		|	        	|
            allocString |           +---------+	        |resetProfiling 	|
                        |	    |			|			|
                       \|/	   \|/ 	       	       \|/		       \|/
                    +-------------------+        +--------------+            +------------+
                    |String             | 	 |Profiling     | 	     |Math        |
                    +-------------------+ 	 +--------------+ 	     +------------+
                    | explode           | 	 | profileTick  | 	     | mkReal     |
                    | allocString       |---+    | profileOn    | 	     | deReal     |
                    | ...               |   |    | profileOff   | 	     | ...        |
                    +-------------------+   |    | ...          | 	     +------------+
	                          |         |    +--------------+
	     		          |	    |		 |
	     	     explodeString|	    |		 |
	     	                 \|/        |alloc     	 |Ro*, ect.
                             +----------+   +----+       |
                             |List      |    	 | 	 |
                             +----------+    	 | 	 |
                             | mkCons   |    	 | 	 |
                             | mkNil    |    	 | 	 |
                             | ...      |    	 | 	 |
                             +----------+    	 | 	 |
				  | 	     	 | 	 |
 				  |	     	\|/	\|/
                                  |alloc     +-------------------+
                                  |          |Region.c/Region.h  |
                                  |          +-------------------+
                                  +--------->| alloc             |
                                             | allocRegion       |
                                             | ...               |
                                             +-------------------+


*/


/*----------------------------------------------------------------*
 * External declarations                                          *
 *----------------------------------------------------------------*/
extern int profType;            /* From Profiling.c */
extern int profNo;              /* From Profiling.c */
extern int microsec;            /* From Profiling.c */
extern int sec;                 /* From Profiling.c */
extern int noTimer;             /* From Profiling.c */
extern int verboseProfileTick;  /* From Profiling.c */
extern int printProfileTab;     /* From Profiling.c */
extern int timeToProfile;       /* From Profiling.c */

extern struct itimerval rttimer;     /* From Profiling.c */
extern struct itimerval old_rttimer; /* From Profiling.c */
extern struct sigvec vec;            /* From Profiling.c */
extern struct sigvec ovec;           /* From Profiling.c */

extern unsigned int lastCpuTime; /* From Profiling.c */
extern char logName[100];        /* From Profiling.c */


/*----------------------------------------------------------------*
 *        Prototypes for external and internal functions.         *
 *----------------------------------------------------------------*/

#if TAG_VALUES
int equalPoly(int x, int y);
#endif

int die (char *);
int terminate (int status);    /* status is an ML value */
void uncaught_exception (StringDesc *exnStr);


#endif /*RUNTIME*/

