/*----------------------------------------------------------------*
 *             Runtime system for the ML-Kit                      *
 *----------------------------------------------------------------*/
#include <stdio.h>
#include <signal.h>
#include <stdlib.h> 
#include "Runtime.h"
#include "Flags.h"
#include "Tagging.h"
#include "String.h"
#include "Math.h"
#include "Exception.h"
#include "Region.h"
#include "Table.h"
#include "CommandLine.h"

#ifdef ENABLE_GC
#include "GC.h"
#endif

#ifdef PROFILING
#include "Profiling.h"
#endif

int 
die (char *s) 
{ 
  fprintf(stdout,"Runtime Error: %s\n",s); 
  exit(-1); 
}

int 
die2 (char *s1, char* s2) 
{ 
    fprintf(stderr,"Runtime Error: %s\n%s",s1,s2); 
    exit(-1); 
}

int 
terminateML (int status) 
{ 
#ifdef ENABLE_GC
  extern int report_gc;
  extern int num_gc;
  extern int gc_total;
  extern unsigned int alloc_total;
  extern unsigned int alloc_period;
  extern double FRAG_sum;
#endif
  debug(printf("[terminateML..."));

#ifdef PROFILING
  outputProfilePost();
  Statistics();        
#endif

#ifdef ENABLE_GC
  if ( report_gc )
    { 
      alloc_total += alloc_period;
      fprintf(stderr, "[GC: %d garbage collections]\n", num_gc);
    }
  if ( verbose_gc ) 
    {
      double ri = 0.0;
      double gc = 0.0;
      alloc_total += alloc_period;
      alloc_total += lobjs_period;
      gc = 100.0 * ((double)gc_total) / ((double)alloc_total);
      ri = 100.0 - gc;
      fprintf(stderr, "[GC: %d garbage collections, RI: %4.1f%, GC: %4.1f%, Frag avg: %4.1f%]\n", num_gc, 
	      ri, gc, FRAG_sum / (double)(num_gc-1));
    }
#endif /* ENABLE_GC */

  debug(printf("]\n"));

  exit (convertIntToC(status)); 
}

void 
uncaught_exception (StringDesc *exnStr) 
{ 
  fprintf(stderr,"uncaught exception "); 
  fflush(stderr);
  outputStream(stderr, exnStr);
  fprintf(stderr, "\n"); 
  fflush(stderr);

#ifdef PROFILING
  outputProfilePost();
  Statistics();        
#endif

  exit (-1); 
}

#ifdef TAG_VALUES

static inline int 
equalTable(Table *x, Table *y) 
{
  int i, sz_x, *px, *py;
  sz_x = get_table_size(x->size);
  if ( sz_x != get_table_size(y->size))
    {
      return mlFALSE;
    }
  px = &(x->data); 
  py = &(y->data); 
  for ( i = 0 ; i < sz_x ; i ++ )
    {
      if ( equalPolyML(*(px+i), *(py+i)) == mlFALSE )
	{
	  return mlFALSE;
	}
    }
  return mlTRUE;
}

/*----------------------------------------------------------------------*
 *equalPolyML:                                                            *
 *----------------------------------------------------------------------*/
int 
equalPolyML(int x, int y) 
{
  int i;

L0:
  /* if x and y are equal then return true (unboxed or boxed) */
  if (x == y) return mlTRUE;

  /* x and y are unboxed if the first bit is set... */
  if (x & 1) {
    if (y & 1) {
      return mlFALSE;
    } else die("equal_polyF - unboxed vs. boxed...");
  } else {
    if (y & 1) die("equal_polyF - boxed vs. unboxed...");

    /* comparing two boxed values */
    /* if tags are distinct then return mlFALSE */
    if (val_tag_kind(x) != val_tag_kind(y)) {
      return mlFALSE;
    }

    /* now one knows that the tags are equal */
    if (val_tag_kind(x) == TAG_CON0) { /* Why not check the constructor tags? 10/01/1999, Niels */
      return mlTRUE;
    }
    if (val_tag_kind(x) == TAG_CON1) { /* Why not check the constructor tags? 10/01/1999, Niels */
      x = *(((int *)x)+1);
      y = *(((int *)y)+1);
      goto L0;
    }
    /*    if (valTagKind(x) == valueTagReal) {
      if (get_d(x) == get_d(y))
	return mlTRUE;
	else return mlFALSE; 
    }Obsolete 10/01/1999, Niels */
    if (val_tag_kind(x) == TAG_STRING) {
      return equalStringML((StringDesc *) x, (StringDesc *) y);
    }
    if (val_tag_kind(x) == TAG_RECORD) {
      for (i = 1; i <= get_record_size(x); i++) {
	if (equalPolyML(*(((int *)x)+i), *(((int *)y)+i)) == mlFALSE)
	  return mlFALSE;
      }
      return mlTRUE;
    }
    if (val_tag_kind(x) == TAG_REF) {
      if ((((int *)x)+1) == (((int *)y)+1)) 
	return mlTRUE;
      else 
	return mlFALSE;
    }
    if (val_tag_kind(x) == TAG_TABLE) {
      return equalTable((Table*)x,(Table*)y);
    }
    die("equal_poly - No matching tag!");
  }
} 

#endif /* TAG_VALUES */

void 
sig_handler_int(void) 
{
  signal(SIGINT, (SignalHandler)sig_handler_int);    /* setup handler again */

#ifdef ENABLE_GC
  if ( doing_gc ) {
    raised_exn_interupt=1;
    return;
  }
#endif /* ENABLE_GC */

#ifdef PROFILING
  if ( doing_prof ) {
    raised_exn_interupt_prof=1;
    return;
  }
#endif /* PROFILING */

  raise_exn((int)&exn_INTERRUPT);
  return; /* never comes here */
}

void 
sig_handler_fpe(void) 
{
  signal(SIGFPE, (SignalHandler)sig_handler_fpe);    /* setup handler again */

#ifdef ENABLE_GC
  if ( doing_gc ) {
    raised_exn_overflow=1;
    return;
  }
#endif /* ENABLE_GC */

#ifdef PROFILING
  if ( doing_prof ) {
    raised_exn_overflow_prof=1;
    return;
  }
#endif /* PROFILING */

  raise_exn((int)&exn_OVERFLOW);
  return; /* never comes here */
}

int 
main(int argc, char *argv[]) 
{
  if ((((double)Max_Int) != Max_Int_d) || (((double)Min_Int) != Min_Int_d))
    die("integer configuration is erroneous");

  parseCmdLineArgs(argc, argv);   /* also initializes ml-access to args */

#ifdef PROFILING
  resetProfiler();
#endif

  /* setup handlers */
  signal(SIGINT, (SignalHandler)sig_handler_int);
  signal(SIGFPE, (SignalHandler)sig_handler_fpe);

  debug(printf("Starting execution...\n");)
#ifdef KAM
  return (main_interp(argc, argv));
#else
  code();
  return (0);   /* never comes here (i.e., exits through 
		 * terminateML or uncaught_exception) */
#endif
}
