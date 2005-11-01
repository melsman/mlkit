#include <stdlib.h>
#include <string.h>

#include "CommandLine.h"
#include "String.h"
#include "List.h"
#include "Tagging.h"
#include "Flags.h"
#include "Profiling.h"

#ifdef PROFILING
#include <signal.h>      /* Used by signal. */
#include <sys/time.h>    /* Used by setitimer. */
#endif

int commandline_argc;     // Kam-backend (Interp.c) need access to update these variables
char **commandline_argv;  // when discharging object file arguments.
static int app_arg_index = 1; /* index for first argument to application. Set by parseArgs */
// static char exeName[100];

/*----------------------------------------*
 * Flags recognized by the runtime system *
 *----------------------------------------*/
#ifdef ENABLE_GC
int disable_gc = 0;
int verbose_gc = 0;
int report_gc = 0;
double heap_to_live_ratio = HEAP_TO_LIVE_RATIO;
#endif

void printUsage(void) {
  fprintf(stderr,"Usage: %s\n", commandline_argv[0]);
  fprintf(stderr,"      [-help, -h] \n");
#ifdef ENABLE_GC
  fprintf(stderr,"      [-disable_gc | -verbose_gc | -report_gc] [-heap_to_live_ratio d] \n");
#endif /*ENABLE_GC*/
#ifdef PROFILING
  fprintf(stderr,"      [-notimer n | -realtime | -virtualtime | -profiletime] \n");
  fprintf(stderr,"      [-microsec n | -sec n] \n");
  fprintf(stderr,"      [-file outFileName] [-noDatafile] [-showStat] \n");
  fprintf(stderr,"      [-profTab] [-verbose] \n\n");
#endif /*PROFILING*/
  fprintf(stderr,"  where\n");
  fprintf(stderr,"      -help, -h                Print this help screen and exit.\n\n");
#ifdef ENABLE_GC
  fprintf(stderr,"      -disable_gc              Disable garbage collector.\n");
  fprintf(stderr,"      -verbose_gc              Show info after each garbage collection.\n");
  fprintf(stderr,"      -report_gc               Show info when program terminates.\n");
  fprintf(stderr,"      -heap_to_live_ratio d    Use heap to live ratio d, ex. 3.0.\n\n");
#endif /*ENABLE_GC*/
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

void parseCmdLineArgs(int argc, char *argv[]) {
  int match;

  /* initialize global variables to hold command line arguments */
  commandline_argc = argc;
  commandline_argv = argv;

#if ( PROFILING || ENABLE_GC )
  //  strcpy(exeName, (char *)argv[0]);
  match = 1;
  while ((--argc > 0) && match) {
    ++argv;    /* next parameter. */
    match = 0;

    if ((strcmp((char *)argv[0], "-h")==0) ||
	(strcmp((char *)argv[0], "-help")==0)) {
      match = 1;
      printUsage();  /* exits */
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

    if (match) { 
      app_arg_index++;
    }
  }
#endif /* PROFILING || ENABLE_GC */  

  return;
}

String
REG_POLY_FUN_HDR(sml_commandline_name, Region rAddr) 
{
  return REG_POLY_CALL(convertStringToML, rAddr, commandline_argv[0]);
} 

int 
REG_POLY_FUN_HDR(sml_commandline_args, Region pairRho, Region strRho) 
{
  int *resList, *pairPtr;
  String mlStr;
  int counter = commandline_argc;
  makeNIL(resList);  
  while ( counter > app_arg_index ) 
    {
      mlStr = REG_POLY_CALL(convertStringToML, strRho, commandline_argv[--counter]);
      REG_POLY_CALL(allocPairML, pairRho, pairPtr);
      first(pairPtr) = (int) mlStr;
      second(pairPtr) = (int) resList;
      makeCONS(pairPtr, resList);
    }
  return (int) resList;
}
