/*----------------------------------------------------------------*
 *             Runtime system for the ML-Kit                      *
 *----------------------------------------------------------------*/
#include <stdio.h>
#include <signal.h>
#include <stdlib.h> /*25/03/1997, Niels*/
#include "Runtime.h"
#include "Flags.h"
#include "Tagging.h"
#include "String.h"
#include "Math.h"
#include "Exception.h"


#ifdef PROFILING
#include "Profiling.h"
#endif

int commandline_argc;
char **commandline_argv;

/*----------------------------------------------------------------------*
 *                                 DIE                                  *
 *----------------------------------------------------------------------*/
int die (char *s) 
{ 
    fprintf(stderr,"Runtime Error: %s\n",s); 
    exit(-1); 
}

int terminate (int status) 
{ 
#ifdef PROFILING
  outputProfile();
  Statistics();        
#endif
  exit (convertIntToC(status)); 
}

void uncaught_exception (StringDesc *exnStr)
{ 
    fprintf(stderr,"uncaught exception "); 
    fflush(stderr);
    outputStream(stderr, exnStr);
    fprintf(stderr, "\n"); 
    fflush(stderr);
    exit(0); 
}

#if TAG_VALUES

/*----------------------------------------------------------------------*
 *equalPoly:                                                            *
 *----------------------------------------------------------------------*/
int equalPoly(int x, int y)
{
  int i;

#if DEBUG_EQUAL_POLY
  printf("Entering function equal_poly.\n");
#endif

L0:
  /* if x and y are equal then return true (unboxed or boxed) */
  if (x == y) return mlTRUE;

  /* x and y are unboxed if the first bit is set... */
  if (x & 1) {
    if (y & 1) {
      #if DEBUG_EQUAL_POLY
      printf("Different unboxed values.\n");
      #endif
      return mlFALSE;
    } else die("equal_polyF - unboxed vs. boxed...");
  } else {
    if (y & 1) die("equal_polyF - boxed vs. unboxed...");

    /* comparing two boxed values */
    /* if tags are distinct then return mlFALSE */
    if (valTag(x) != valTag(y)) {
      #if DEBUG_EQUAL_POLY
      printf("Different tags.\n");
      #endif
      return mlFALSE;
    }

    /* now one knows that the tags are equal */
    if (valTagKind(x) == valueTagCon0) {
      #if DEBUG_EQUAL_POLY
      printf("Same nullary constructor.\n");
      #endif
      return mlTRUE;
    }
    if (valTagKind(x) == valueTagCon1) {
      #if DEBUG_EQUAL_POLY
      printf("Compare constructor argument of unary constructor.\n");
      #endif
      x = *(((int *)x)+1);
      y = *(((int *)y)+1);
      goto L0;
    }
    if (valTagKind(x) == valueTagReal) {
      #if DEBUG_EQUAL_POLY
      printf("Compare real values.\n");
      #endif
      if (get_d(x) == get_d(y))
	return mlTRUE;
      else return mlFALSE;
    }
    if (valTagKind(x) == valueTagString) {
      #if DEBUG_EQUAL_POLY
      printf("Compare strings.\n");
      #endif
      return equalString((StringDesc *) x, (StringDesc *) y);
    }
    if (valTagKind(x) == valueTagRecord) {
      for (i = 1; i <= size_record(x); i++) {
        #if DEBUG_EQUAL_POLY
	printf("Compare %d'th. component of record.\n", i);
        #endif	
	if (equalPoly(*(((int *)x)+i), *(((int *)y)+i)) == mlFALSE)
	  return mlFALSE;
      }
      return mlTRUE;
    }
    if (valTagKind(x) == valueTagRef) {
      #if DEBUG_EQUAL_POLY
      printf("Compare refs.\n");
      #endif
      if ((((int *)x)+1) == (((int *)y)+1)) return mlTRUE;
      else return mlFALSE;
    }
    die("equal_poly - No matching tag!");
  }
} 

#endif


/* Exception handlers */

void sig_handler_int(void)
{
/*  printf("INTERRUPT\n"); */
  signal(SIGINT, (SignalHandler)sig_handler_int);    /* setup handler again */
  raise_exn((int)&Interrupt_val);
  return; /* never comes here */
}

void sig_handler_fpe(void)
{
/*  printf("OVERFLOW\n"); */
  signal(SIGFPE, (SignalHandler)sig_handler_fpe);    /* setup handler again */
  raise_exn((int)&Overflow_val);
  return; /* never comes here */
}


int main(int argc, char *argv[])
{
  /* initialize global variables to hold command line arguments */
  commandline_argc = argc;
  commandline_argv = argv;

  if ((((double)Max_Int) != Max_Int_d) || (((double)Min_Int) != Min_Int_d))
    die("integer configuration is erroneous");

#ifdef PROFILING
  /* Perhaps checkArgs should remove used arguments from commandline_argv.. */
  checkArgs(argc, argv);
  resetProfiler();
#endif

  /* setup handlers */
  signal(SIGINT, (SignalHandler)sig_handler_int);
  signal(SIGFPE, (SignalHandler)sig_handler_fpe);

/*  printf("[Starting execution...\n"); */
  code();

/*  printf("Finish execution...]\n"); */
#ifdef PROFILING
  Statistics();
#endif

  return (0);

}
