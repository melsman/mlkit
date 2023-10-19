#include <stdlib.h>
#include <string.h>

#include "CommandLine.h"
#include "String.h"
#include "List.h"
#include "Tagging.h"
#include "Flags.h"
#include "Profiling.h"

#ifdef ARGOBOTS
#include "Spawn.h"
#include <unistd.h>
#endif

#ifdef PROFILING
#include <signal.h>      /* Used by signal. */
#include <sys/time.h>    /* Used by setitimer. */
#endif

int commandline_argc;     // Kam-backend (Interp.c) needs access to update these variables
char **commandline_argv;  // when discharging object file arguments.
static int app_arg_index = 1; /* index for first argument to application. Set by parseArgs */
// static char exeName[100];

char * command_pipe = NULL;  // Named command pipe for REPL
char * reply_pipe = NULL;    // Named reply pipe for REPL
char * repl_logfile = NULL;  // Name of REPL log file

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
  fprintf(stderr,"Usage: %s\n", commandline_argv[0]);
  fprintf(stderr,"      [-help, -h] \n");
  fprintf(stderr,"      [-command_pipe n] \n");
  fprintf(stderr,"      [-reply_pipe n] \n");
  fprintf(stderr,"      [-repl_logfile n] \n");
#ifdef ENABLE_GC
  fprintf(stderr,"      [-disable_gc | -verbose_gc | -report_gc] [-heap_to_live_ratio d] \n");
#ifdef ENABLE_GENGC
  fprintf(stderr,"      [-only_major_gc] \n");
#endif // ENABLE_GEN_GC
#endif /*ENABLE_GC*/
#ifdef PROFILING
  fprintf(stderr,"      [-notimer n | -realtime | -virtualtime | -profiletime] \n");
  fprintf(stderr,"      [-microsec n | -sec n] \n");
  fprintf(stderr,"      [-file outFileName] [-noDatafile] [-showStat] \n");
  fprintf(stderr,"      [-profTab] [-verbose] \n");
#endif /*PROFILING*/
#if (PARALLEL && ARGOBOTS)
  fprintf(stderr,"      [-p n] [-verbose_par, -vp] \n");
#endif
  fprintf(stderr,"  where\n");
  fprintf(stderr,"      -help, -h                Print this help screen and exit.\n\n");
  fprintf(stderr,"      -command_pipe n          Named pipe for REPL commands.\n\n");
  fprintf(stderr,"      -reply_pipe n            Named pipe for REPL replies.\n\n");
  fprintf(stderr,"      -repl_logfile n          Name of REPL log file.\n\n");
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
#ifdef PROFILING
  fprintf(stderr,"      -notimer n               Profile every n'th function call.\n");
  fprintf(stderr,"      -realtime                Profile with the real timer.\n");
  fprintf(stderr,"      -virtualtime             Profile with the virtual timer.\n");
  fprintf(stderr,"      -profiletime             Profile with the profile timer.\n\n");
  fprintf(stderr,"      -microsec n              If a timer is chosen, then profile\n");
  fprintf(stderr,"                                  every n'th microsecond.\n");
  fprintf(stderr,"      -sec n                   If a timer is chosen, then profile\n");
  fprintf(stderr,"                                   every n'th second.\n\n");
  fprintf(stderr,"      -file outFileName        Use outFileName as profile datafile, \n");
  fprintf(stderr,"                                  default is %s\n\n", logName);
  fprintf(stderr,"      -profTab                 Print profiling table.\n");
  fprintf(stderr,"      -verbose                 Verbose mode.\n");
  fprintf(stderr,"      -noDatafile              The profile datafile is not exported,\n");
  fprintf(stderr,"                                  default is to export the datafile.\n");
  fprintf(stderr,"      -showStat                Statistics are shown after execution.\n\n");
  fprintf(stderr,"  This program is compiled using the ML Kit with profiling\n");
  fprintf(stderr,"  enabled. When executed, the program generates a profile\n");
  fprintf(stderr,"  datafile, which can be passed to the program `rp2ps' to\n");
  fprintf(stderr,"  create PostScript profile graphs.\n\n");
#endif /*PROFILING*/
  exit(0);
}

void
parseCmdLineArgs(int argc, char *argv[])
{
  long match;

#ifdef ARGOBOTS
  posixThreads = (int)sysconf(_SC_NPROCESSORS_ONLN);
  int verbosePar = 0;
#endif

  /* initialize global variables to hold command line arguments */
  commandline_argc = argc;
  commandline_argv = argv;

  match = 1;
  while ((--argc > 0) && match) {
    ++argv;    /* next parameter. */
    match = 0;

    if ((strcmp((char *)argv[0], "-h")==0) ||
	(strcmp((char *)argv[0], "-help")==0)) {
      match = 1;
      printUsage();  /* exits */
    }

    if (strcmp((char *)argv[0],"-command_pipe")==0) {
      if (--argc > 0 && (*++argv)[0]) { /* Is there an argument? */
	command_pipe = (char *)argv[0];
      } else {
	fprintf(stderr,"Missing argument to -command_pipe switch.\n");
	printUsage();
      }
      app_arg_index++; /* two-word option */
      match = 1;
    }

    if (strcmp((char *)argv[0],"-reply_pipe")==0) {
      if (--argc > 0 && (*++argv)[0]) { /* Is there an argument? */
	reply_pipe = (char *)argv[0];
      } else {
	fprintf(stderr,"Missing argument to -reply_pipe switch.\n");
	printUsage();
      }
      app_arg_index++; /* two-word option */
      match = 1;
    }

    if (strcmp((char *)argv[0],"-repl_logfile")==0) {
      if (--argc > 0 && (*++argv)[0]) { /* Is there an argument? */
	repl_logfile = (char *)argv[0];
      } else {
	fprintf(stderr,"Missing argument to -repl_logfile switch.\n");
	printUsage();
      }
      app_arg_index++; /* two-word option */
      match = 1;
    }

#ifdef ENABLE_GC
    if (strcmp((char *)argv[0],"-disable_gc")==0) {
      disable_gc = 1;
      match = 1;
    }

    if (strcmp((char *)argv[0],"-verbose_gc")==0) {
      verbose_gc = 1;
      match = 1;
    }

    if (strcmp((char *)argv[0],"-report_gc")==0) {
      report_gc = 1;
      match = 1;
    }

#ifdef ENABLE_GEN_GC
    if (strcmp((char *)argv[0],"-only_major_gc")==0) {
      only_major_gc = 1;
      match = 1;
    }
#endif // ENABLE_GEN_GC

    if (strcmp((char *)argv[0],"-heap_to_live_ratio")==0) {
      if (--argc > 0 && (*++argv)[0]) { /* Is there a number. */
	if ((heap_to_live_ratio = atof((char *)argv[0])) == 0) {
	  fprintf(stderr,"Something wrong with the double in switch -heap_to_live_ratio.\n");
	  printUsage();
	}
      } else {
	fprintf(stderr,"No double after the switch heap_to_live_ratio.\n");
	printUsage();
      }
      app_arg_index++; /* this is an two-word option */
      match = 1;
    }
#endif /*ENABLE_GC*/
#ifdef PROFILING
    if (strcmp((char *)argv[0],"-notimer")==0) {
      profType = noTimer;
      match = 1;
      app_arg_index++; /* this is an two-word option */
      if (--argc > 0 && (*++argv)[0]) { /* Is there a number. */
	if ((profNo = atoi((char *)argv[0])) == 0) {
	  fprintf(stderr,"Something wrong with the number no in switch -notimer no.\n");
	  printUsage();
	}
      } else {
	fprintf(stderr,"No number after the switch -notimer.\n");
	printUsage();
      }
    }

    if (strcmp((char *)argv[0],"-realtime")==0) {
      profType = ITIMER_REAL;
      signalType = SIGALRM;
      match = 1;
    }

    if (strcmp((char *)argv[0],"-virtualtime")==0) {
      profType = ITIMER_VIRTUAL;
      signalType = SIGVTALRM;
      match = 1;
    }

    if (strcmp((char *)argv[0],"-profiletime")==0) {
      profType = ITIMER_PROF;
      signalType = SIGPROF;
      match = 1;
    }

    if (strcmp((char *)argv[0],"-profTab")==0) {
      printProfileTab = 1;
      match = 1;
    }

    if (strcmp((char *)argv[0],"-microsec")==0) {
      match = 1;
      app_arg_index++; /* this is an two-word option */
      if (--argc > 0 && (*++argv)[0]) { /* Is there a number. */
	if ((microsec = atoi((char *)argv[0])) == 0) {
	  fprintf(stderr,"Something wrong with the number no in switch -microsec no.\n");
	  printUsage();
	}
	sec = 0;
      } else {
	fprintf(stderr,"No number after the switch -microsec.\n");
	printUsage();
      }
    }

    if (strcmp((char *)argv[0],"-sec")==0) {
      match = 1;
      app_arg_index++; /* this is an two-word option */
      if (--argc > 0 && (*++argv)[0]) { /* Is there a number. */
	if ((sec = atoi((char *)argv[0])) == 0) {
	  fprintf(stderr,"Something wrong with the number no in switch -sec no.\n");
	  printUsage();
	}
	microsec = 0;
      } else {
	fprintf(stderr,"No number after the switch -sec.\n");
	printUsage();
      }
    }

    if (strcmp((char *)argv[0],"-file")==0) {
      match = 1;
      app_arg_index++; /* this is an two-word option */
      if ((argc-1)>0 && (*(argv+1))[0] != '-') {
	--argc;
	++argv;
	strcpy(logName, (char *)argv[0]);
      } else {
	fprintf(stderr,"No filename after the -file switch.\n");
	printUsage();
      }
      fprintf(stderr,"Using output file %s.\n", logName);
    }

    if ((strcmp((char *)argv[0], "-v")==0) ||
	(strcmp((char *)argv[0], "-verbose")==0)) {
      match = 1;
      verboseProfileTick = 1;
    }

    if ((strcmp((char *)argv[0], "-noDatafile")==0) ||
	(strcmp((char *)argv[0], "-noProfileDatafile")==0)) {
      match = 1;
      exportProfileDatafile = 0;
    }

    if ((strcmp((char *)argv[0], "-showStat")==0) ||
	(strcmp((char *)argv[0], "-showStatistics")==0)) {
      match = 1;
      showStat = 1;
    }
#endif /*PROFILING*/

#ifdef ARGOBOTS
    if (strcmp((char *)argv[0],"-p")==0) {
      if (--argc > 0 && (*++argv)[0]) { /* Is there a number. */
	if ((posixThreads = atoi((char *)argv[0])) == 0) {
	  fprintf(stderr,"Expecting integer argument to the option -p.\n");
	  printUsage();
	}
	if (posixThreads < 1) {
	  fprintf(stderr,"Expecting positive integer after option -p.\n");
	  printUsage();
	}
      } else {
	fprintf(stderr,"Expecting integer after the option -p.\n");
	printUsage();
      }
      app_arg_index++; /* this is an two-word option */
      match = 1;
    }

    if ((strcmp((char *)argv[0], "-vp")==0) ||
	(strcmp((char *)argv[0], "-verbose_par")==0)) {
      match = 1;
      verbosePar = 1;
    }
#endif

    if (match) {
      app_arg_index++;
    }
  }

#if (PARALLEL && ARGOBOTS)
  if (verbosePar) {
    printf("ARGOBOTS: Using %d execution streams.\n", posixThreads);
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
  while ( counter > app_arg_index )
    {
      mlStr = REG_POLY_CALL(convertStringToML, strRho, commandline_argv[--counter]);
      REG_POLY_CALL(allocPairML, pairRho, pairPtr);
      first(pairPtr) = (size_t) mlStr;
      second(pairPtr) = (size_t) resList;
      makeCONS(pairPtr, resList);
    }
  return (uintptr_t) resList;
}
