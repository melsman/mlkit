/*----------------------------------------------------------------*
 *                  COMMAND LINE                                  *
 *----------------------------------------------------------------*/

#ifndef COMMAND_LINE_H
#define COMMAND_LINE_H

#include <stdio.h>
#include "Flags.h"
#include "String.h"

/* 

The executable run may take command line arguments to either the
runtime system or the application. Command line arguments for the
application are accessed through the basis library using the
sml_commandline_name and sml_commandline_args primitives. The
variables commandline_argv and commandline_argc are initialized to
point at the first argument for the application. We assume arguments
for the runtime system are first. For instance, in

  run -tags_only -arg1

the first commandline argument for the application is -arg1 because
-tags_onlyKit is recognized as a Kit command line argument.
 
*/

/*----------------------------------------*
 * Flags recognized by the runtime system *
 *----------------------------------------*/
extern int disable_gc;
extern int verbose_gc;
extern int report_gc;
extern double heap_to_live_ratio;

/*----------------------------------------*
 * Prototypes                             *
 *----------------------------------------*/
#ifdef PROFILING
String sml_commandline_nameProfiling(Region rAddr, int pPoint);
int sml_commandline_argsProfiling(Region pairRho, Region strRho, int pPoint);
#else
String sml_commandline_name(Region rAddr);
int sml_commandline_args(Region pairRho, Region strRho);
#endif /* PROFILING */

void parseCmdLineArgs(int argc, char *argv[]);

#endif /* COMMAND_LINE_H */
