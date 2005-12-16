/*----------------------------------------------------------------*
 *             Runtime system for the ML-Kit                      *
 *----------------------------------------------------------------*/

#ifndef RUNTIME_H
#define RUNTIME_H

#include "String.h"
#include "Flags.h"

/* Structure of the runtime system is as follows:

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
 *        Prototypes for external and internal functions.         *
 *----------------------------------------------------------------*/

#ifdef TAG_VALUES
int equalPolyML(int x, int y);
#endif

int die (char *);
int die2 (char *, char *);
int terminate (int status);    /* status is a C value */
int terminateML (int status);  /* status is an ML value */
void uncaught_exception (StringDesc *exnStr, unsigned long, int);

#endif /* RUNTIME_H */

