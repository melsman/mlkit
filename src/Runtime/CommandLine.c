#include <stdlib.h>
#include <string.h>

#include "CommandLine.h"
#include "String.h"
#include "List.h"
#include "Tagging.h"
#include "Flags.h"
#include "RegionProfile.h"

#ifdef ARGOBOTS
#include "Spawn.h"
#include <unistd.h>
#endif


int commandline_argc;     // Kam-backend (Interp.c) needs access to update these variables
char **commandline_argv;  // when discharging object file arguments.
// static char exeName[100];

const RuntimeOptionExtension *runtime_options = NULL;
int mlkit_repl_mode = 0;
/* The REPL module supplies the override; normal executables need no Repl.o. */
__attribute__((weak)) void mlkit_init_runtime_options(void) {}

/*----------------------------------------*
 * Flags recognized by the runtime system *
 *----------------------------------------*/
#ifdef ENABLE_GC
long disable_gc = 0;
long verbose_gc = 0;
long report_gc = 0;
double heap_to_live_ratio = HEAP_TO_LIVE_RATIO;
#ifdef ENABLE_GEN_GC
long only_major_gc = 0;
#endif
#endif

void
printUsage(void)
{
  fprintf(stderr,"Usage: %s [application arguments] [+RTS runtime options -RTS]\n", commandline_argv[0]);

#ifdef PROFILING
  fprintf(stderr,"      [-rp [-rp_file PATH] [-rp_paused] [-rp_interval Nus|Nms|Ns|0]");
#ifdef ENABLE_GC
  fprintf(stderr," [-rp_gc_samples]");
#endif
  fprintf(stderr," [-rp_report]]\n");
  fprintf(stderr,"      [-rp_region all|UNIT:BINDING [-rp_build ID]]\n");
#endif
  fprintf(stderr,"      -RTS ends a runtime block; --RTS ends runtime parsing.\n");
  fprintf(stderr,"      -- ends runtime parsing and is passed to the application.\n");
  fprintf(stderr,"      [-help, -h] \n");
#ifdef ENABLE_GC
  fprintf(stderr,"      [-disable_gc | -verbose_gc | -report_gc] [-heap_to_live_ratio d] \n");
#ifdef ENABLE_GEN_GC
  fprintf(stderr,"      [-only_major_gc] \n");
#endif // ENABLE_GEN_GC
#endif /*ENABLE_GC*/
#if (PARALLEL && ARGOBOTS)
  fprintf(stderr,"      [-p n] [-verbose_par, -vp] \n");
#endif
  fprintf(stderr,"  where\n");
  fprintf(stderr,"      -help, -h                Print this help screen and exit.\n\n");
#ifdef ENABLE_GC
  fprintf(stderr,"      -disable_gc              Disable garbage collector.\n");
  fprintf(stderr,"      -verbose_gc              Show info after each garbage collection.\n");
  fprintf(stderr,"      -report_gc               Show info when program terminates.\n");
  fprintf(stderr,"      -heap_to_live_ratio d    Use heap to live ratio d (default: %f).\n", heap_to_live_ratio);
#ifdef ENABLE_GEN_GC
  fprintf(stderr,"      -only_major_gc           Use only major collections.\n");
#endif // ENABLE_GEN_GC
  fprintf(stderr, "\n");
#endif /*ENABLE_GC*/
#ifdef ARGOBOTS
  fprintf(stderr,"      -p n                     Number of execution streams.\n");
  fprintf(stderr,"      -verbose_par, -vp        Show info about parallel streams.\n\n");
#endif
  if (runtime_options) runtime_options->usage();
  exit(0);
}

/* Delimiters cannot be consumed as option values. */
static int rtsDelimiter(const char *arg) {
  return !strcmp(arg,"+RTS") || !strcmp(arg,"-RTS") ||
         !strcmp(arg,"--RTS") || !strcmp(arg,"--");
}
char *rtsValue(int *argc, char ***argv) {
  if (*argc <= 1 || rtsDelimiter((*argv)[1])) {
    fprintf(stderr,"Missing argument to runtime option %s\n", (*argv)[0]);
    exit(EXIT_FAILURE);
  }
  --*argc;
  return *++*argv;
}

void
parseCmdLineArgs(int argc, char *argv[])
{
  mlkit_init_runtime_options();
  long match;
  int in_rts = 0, finished = 0;
#ifdef PROFILING
  int rp_options = 0;
#endif

#ifdef ARGOBOTS
  posixThreads = (int)sysconf(_SC_NPROCESSORS_ONLN);
  int verbosePar = 0;
#endif

  /* initialize global variables to hold command line arguments */
  commandline_argv = malloc(((size_t)argc + 1) * sizeof(char *));
  if (!commandline_argv) { perror("runtime arguments"); exit(EXIT_FAILURE); }
  commandline_argv[0] = argv[0];
  commandline_argc = 1;

  while (--argc > 0) {
    ++argv;
    if (!finished) {
      if (!strcmp(argv[0],"--RTS")) { finished = 1; continue; }
      if (!strcmp(argv[0],"--")) { finished = 1; in_rts = 0; }
      else if (!strcmp(argv[0],"+RTS")) { in_rts = 1; continue; }
      else if (!strcmp(argv[0],"-RTS")) { in_rts = 0; continue; }
    }
    if (finished || !in_rts) {
      commandline_argv[commandline_argc++] = argv[0];
      continue;
    }
    match = 0;

#ifdef PROFILING
    if (strcmp(argv[0], "-rp") == 0) { mlkit_rp_enabled = 1; match = 1; }
    if (strcmp(argv[0], "-rp_paused") == 0) { mlkit_rp_initially_paused = 1; rp_options = 1; match = 1; }
    if (strcmp(argv[0], "-rp_gc_samples") == 0) { mlkit_rp_gc_samples = 1; rp_options = 1; match = 1; }
    if (strcmp(argv[0], "-rp_report") == 0) { mlkit_rp_report = 1; rp_options = 1; match = 1; }
    if (strcmp(argv[0], "-rp_interval") == 0) {
      if (!mlkit_rp_parse_interval(rtsValue(&argc,&argv))) {
        fprintf(stderr, "-rp_interval requires an integer duration Nus, Nms, Ns, or 0\n"); exit(EXIT_FAILURE);
      }
      rp_options = 1;
      match = 1;
      continue;
    }
    if (strcmp(argv[0], "-rp_build") == 0) {
      if (!rtsValue(&argc,&argv)[0]) { fprintf(stderr, "-rp_build requires an identifier\n"); exit(EXIT_FAILURE); }
      mlkit_rp_expected_build = argv[0]; rp_options = 1; match = 1; continue;
    }
    if (strcmp(argv[0], "-rp_region") == 0) {
      if (!rtsValue(&argc,&argv)[0]) { fprintf(stderr, "-rp_region requires all or UNIT:BINDING\n"); exit(EXIT_FAILURE); }
      mlkit_rp_region = argv[0]; rp_options = 1; match = 1; continue;
    }
    if (strcmp(argv[0], "-rp_file") == 0) {
      if (!rtsValue(&argc,&argv)[0]) { fprintf(stderr, "-rp_file requires a path\n"); exit(EXIT_FAILURE); }
      mlkit_rp_filename = argv[0];
      rp_options = 1;
      match = 1;
      continue;
    }
#endif
    if ((strcmp((char *)argv[0], "-h")==0) ||
	(strcmp((char *)argv[0], "-help")==0)) {
      match = 1;
      printUsage();  /* exits */
    }

    if (runtime_options && runtime_options->parse(&argc,&argv)) continue;

#ifdef ENABLE_GC
    if (strcmp((char *)argv[0],"-disable_gc")==0) {
      disable_gc = 1;
      continue;
    }

    if (strcmp((char *)argv[0],"-verbose_gc")==0) {
      verbose_gc = 1;
      continue;
    }

    if (strcmp((char *)argv[0],"-report_gc")==0) {
      report_gc = 1;
      continue;
    }

#ifdef ENABLE_GEN_GC
    if (strcmp((char *)argv[0],"-only_major_gc")==0) {
      only_major_gc = 1;
      continue;
    }
#endif // ENABLE_GEN_GC

    if (strcmp((char *)argv[0],"-heap_to_live_ratio")==0) {
      if (rtsValue(&argc,&argv)[0]) { /* Is there a number. */
	if ((heap_to_live_ratio = atof((char *)argv[0])) == 0) {
	  fprintf(stderr,"Something wrong with the double in switch -heap_to_live_ratio.\n");
	  exit(EXIT_FAILURE);
	}
      } else {
	fprintf(stderr,"No double after the switch heap_to_live_ratio.\n");
	exit(EXIT_FAILURE);
      }
      continue;
    }
#endif /*ENABLE_GC*/

#ifdef ARGOBOTS
    if (strcmp((char *)argv[0],"-p")==0) {
      if (rtsValue(&argc,&argv)[0]) { /* Is there a number. */
	if ((posixThreads = atoi((char *)argv[0])) == 0) {
	  fprintf(stderr,"Expecting integer argument to the option -p.\n");
	  exit(EXIT_FAILURE);
	}
	if (posixThreads < 1) {
	  fprintf(stderr,"Expecting positive integer after option -p.\n");
	  exit(EXIT_FAILURE);
	}
      } else {
	fprintf(stderr,"Expecting integer after the option -p.\n");
	exit(EXIT_FAILURE);
      }
      continue;
    }

    if ((strcmp((char *)argv[0], "-vp")==0) ||
	(strcmp((char *)argv[0], "-verbose_par")==0)) {
      match = 1;
      verbosePar = 1;
    }
#endif

    if (!match && strncmp(argv[0], "-rp", 3) == 0) {
#ifdef PROFILING
      fprintf(stderr, "unknown profiler option: %s\n", argv[0]);
#else
      fprintf(stderr, "profiler option %s requires an executable compiled with -rp\n", argv[0]);
#endif
      exit(EXIT_FAILURE);
    }
    if (!match) {
      fprintf(stderr,"unknown runtime option: %s\n", argv[0]);
      exit(EXIT_FAILURE);
    }
  }

  commandline_argv[commandline_argc] = NULL;

#if (PARALLEL && ARGOBOTS)
  if (verbosePar) {
    printf("ARGOBOTS: Using %d execution streams.\n", posixThreads);
  }
#endif

#ifdef PROFILING
  if (!mlkit_rp_enabled && rp_options) {
    fprintf(stderr, "profiler options require -rp\n"); exit(EXIT_FAILURE);
  }
#endif
  return;
}

String
REG_POLY_FUN_HDR(sml_commandline_name, Region rAddr)
{
  return REG_POLY_CALL(convertStringToML, rAddr, commandline_argv[0]);
}

uintptr_t
REG_POLY_FUN_HDR(sml_commandline_args, Region pairRho, Region strRho)
{
  uintptr_t *resList, *pairPtr;
  String mlStr;
  int counter = commandline_argc;
  makeNIL(resList);
  while ( counter > 1 )
    {
      mlStr = REG_POLY_CALL(convertStringToML, strRho, commandline_argv[--counter]);
      REG_POLY_CALL(allocPairML, pairRho, pairPtr);
      first(pairPtr) = (size_t) mlStr;
      second(pairPtr) = (size_t) resList;
      makeCONS(pairPtr, resList);
    }
  return (uintptr_t) resList;
}
