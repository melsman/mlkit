/*----------------------------------------------------------------*
 *             Runtime system for the ML-Kit                      *
 *----------------------------------------------------------------*/

#ifndef RUNTIME_H
#define RUNTIME_H

#include "String.h"
#include "Flags.h"
#include "Region.h"

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
uintptr_t equalPolyML(uintptr_t x, uintptr_t y);
#endif

int die (const char *);
int die2 (const char *, const char *);
long terminate (long status);    /* status is a C value */
long terminateML (long status);  /* status is an ML value */
void uncaught_exception (Context ctx, StringDesc *exnStr, unsigned long, uintptr_t);

#endif /* RUNTIME_H */
