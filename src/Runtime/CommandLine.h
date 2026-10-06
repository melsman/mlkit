/*----------------------------------------------------------------*
 *                  COMMAND LINE                                  *
 *----------------------------------------------------------------*/

#ifndef COMMAND_LINE_H
#define COMMAND_LINE_H

#include <stdio.h>
#include "Flags.h"
#include "String.h"

/*

Runtime arguments are enclosed in +RTS ... -RTS blocks. The markers and
runtime options are removed from commandline_argv, which contains the executable
name followed by application arguments in their original order. --RTS ends
runtime parsing and is removed; -- ends parsing and is retained. The Basis
primitives below expose this filtered argument vector.

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
extern char * command_pipe;  // Named command pipe for REPL
extern char * reply_pipe;    // Named reply pipe for REPL

/*----------------------------------------*
 * Prototypes                             *
 *----------------------------------------*/
String REG_POLY_FUN_HDR(sml_commandline_name, Region rAddr);
uintptr_t REG_POLY_FUN_HDR(sml_commandline_args, Region pairRho, Region strRho);

void parseCmdLineArgs(int argc, char *argv[]);

#endif /* COMMAND_LINE_H */
