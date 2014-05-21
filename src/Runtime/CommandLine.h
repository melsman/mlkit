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
-tags_only is recognized as a Kit command line argument.
 
*/

/*----------------------------------------*
 * Flags recognized by the runtime system *
 *----------------------------------------*/
extern long disable_gc;
extern long verbose_gc;
extern long report_gc;
#ifdef ENABLE_GEN_GC
extern long only_major_gc;
#endif
extern double heap_to_live_ratio;

/*----------------------------------------*
 * Prototypes                             *
 *----------------------------------------*/
String REG_POLY_FUN_HDR(sml_commandline_name, Region rAddr);
uintptr_t REG_POLY_FUN_HDR(sml_commandline_args, Region pairRho, Region strRho);

void parseCmdLineArgs(int argc, char *argv[]);

#endif /* COMMAND_LINE_H */
