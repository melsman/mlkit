/*----------------------------------------------------------------*
 *             Runtime system for the ML-Kit                      *
 *----------------------------------------------------------------*/
#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include "Runtime.h"
#include "Flags.h"
#include "Tagging.h"
#include "String.h"
#include "Math.h"
#include "Exception.h"
#include "Region.h"
#include "Table.h"
#include "CommandLine.h"
#include "Export.h"

#ifdef ENABLE_GC
#include "GC.h"
#endif

#ifdef PROFILING
#include "Profiling.h"
#endif

#ifdef KAM
#include "Interp.h"
#endif

int
die (const char *s)
{
  fprintf(stderr,"Runtime Error: %s\n",s);
  fflush(stderr);
  exit(-1);
}

int
die2 (const char *s1, const char* s2)
{
  fprintf(stderr,"Runtime Error: %s\n%s\n",s1,s2);
  fflush(stderr);
  exit(-1);
}

static struct rlimit limit;

void
setStackSize(rlim_t size)
{
  int res;
  char *bad;
  struct rlimit lim;
  struct rlimit oldlim;
  res = getrlimit(RLIMIT_STACK, &oldlim);
  if (res == -1)
  {
    bad = strerror(errno);
    die2("setStackSize(1)", bad);
  }
  lim.rlim_cur = oldlim.rlim_max;
  lim.rlim_max = oldlim.rlim_max;
  res = setrlimit(RLIMIT_STACK, &lim);
  if (res == -1)
  {
    bad = strerror(errno);
    die2("setStackSize(2)", bad);
  }
  res = getrlimit(RLIMIT_STACK, &limit);
  if (res == -1)
  {
    bad = strerror(errno);
    die2("setStackSize(3)", bad);
  }
  // printf("Stack size: %llu; %lluMb\n", limit.rlim_cur, limit.rlim_cur / 1024 / 1024);
  return;
}

void
setStackSizeUnlimited(void)
{
  return setStackSize(RLIM_INFINITY);
}

long
terminateML (long status)
{
  callExportFun("sml_exitCallback", convertIntToML(8)); // exported in Initial2.sml

#ifdef ENABLE_GC
  extern ssize_t gc_total;
  extern ssize_t rp_total;
  extern size_t alloc_total;
  extern size_t alloc_period;
  extern double FRAG_sum;
#endif
  debug(printf("[terminateML..."));

#ifdef PROFILING
  outputProfilePost();
  Statistics();
#endif

#ifdef ENABLE_GC
  if ( report_gc || verbose_gc )
    {
      alloc_total += alloc_period;
      fprintf(stderr, "[GC(%zd.%zdms): %zd collections",
	      time_gc_all_ms / 10, time_gc_all_ms % 10, num_gc);
#ifdef ENABLE_GEN_GC
      fprintf(stderr, ", %zd major (%ld.%ldms)", num_gc_major,
	      time_majorgc_all_ms / 10, time_majorgc_all_ms % 10);
#endif
      fprintf(stderr, ", %zdkb rpages", rp_total);
    }

  if ( verbose_gc )
    {
      double ri = 0.0;
      double gc = 0.0;
      alloc_total += lobjs_period;
      gc = 100.0 * ((double)gc_total) / ((double)alloc_total);
      ri = 100.0 - gc;
      fprintf(stderr, ", RI:%2.0f%%, GC:%2.0f%%, Frag avg:%2.0f%%]\n",
              ri, gc, FRAG_sum / (double)(num_gc-1));
      fflush(stderr);
    }
  else if ( report_gc )
    {
      fprintf(stderr, "]\n");
      fflush(stderr);
    }
#endif /* ENABLE_GC */

  debug(printf("]\n"));

  exit (convertIntToC(status));
}

size_t failNumber = SIZE_MAX;
size_t syserrNumber = SIZE_MAX;

void
sml_setFailNumber(uintptr_t ep, int i)
{
  uintptr_t e = first(ep);
  switch (convertIntToC(i))
  {
    case 1:
      failNumber = convertIntToC(first(e));
      break;
    case 2:
      syserrNumber = convertIntToC(first(e));
      break;
  }
  return;
}

void
uncaught_exception (String exnStr, unsigned long n, uintptr_t ep)
{
  uintptr_t a;
  fprintf(stderr,"uncaught exception ");
  fflush(stderr);
  fputs(&(exnStr->data), stderr);
  fflush(stderr);
  if (convertIntToC(n) == failNumber)
  {
    a = second (ep);
    fputs(" ", stderr);
    fputs(&(((String) a)->data),stderr);
    fflush(stderr);
  }
  if (convertIntToC(n) == syserrNumber)
  {
    a = second(ep);
    a = first(a);
    fputs(" ", stderr);
    fputs(&(((String) a)->data),stderr);
    fflush(stderr);
  }
  fprintf(stderr, "\n");
  fflush(stderr);

#ifdef PROFILING
  outputProfilePost();
  Statistics();
#endif

  exit (-1);
}

#ifdef TAG_VALUES

static inline size_t
equalTable(Table x, Table y)
{
  size_t i, sz_x, *px, *py;
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
uintptr_t
equalPolyML(uintptr_t x, uintptr_t y)
{
  int i;

L0:
  /* if x and y are equal then return true (unboxed or boxed) */
  if (x == y) return mlTRUE;

  /* x and y are unboxed if the first bit is set... */
  if (x & 1) {
    if (y & 1) {
      return mlFALSE;
    } else {
      die("equal_polyF - unboxed vs. boxed...");
      return mlFALSE; // never comes here
    }
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
      x = *(((uintptr_t *)x)+1);
      y = *(((uintptr_t *)y)+1);
      goto L0;
    }
    /*    if (valTagKind(x) == valueTagReal) {
      if (get_d(x) == get_d(y))
        return mlTRUE;
        else return mlFALSE;
    }Obsolete 10/01/1999, Niels */
    if (val_tag_kind(x) == TAG_STRING) {
      return equalStringML((String) x, (String) y);
    }
    if (val_tag_kind(x) == TAG_RECORD) {
      for (i = 1; i <= get_record_size(x); i++) {
        if (equalPolyML(*(((uintptr_t *)x)+i), *(((uintptr_t *)y)+i)) == mlFALSE)
          return mlFALSE;
      }
      return mlTRUE;
    }
    if (val_tag_kind(x) == TAG_REF) {
      if ((((uintptr_t *)x)+1) == (((uintptr_t *)y)+1))
        return mlTRUE;
      else
        return mlFALSE;
    }
    if (val_tag_kind(x) == TAG_TABLE) {
      return equalTable((Table)x,(Table)y);
    }
    die("equal_poly - No matching tag!");
    return mlFALSE;  // never comes here
  }
}

#endif /* TAG_VALUES */

/*
void
sig_handler_segv(int sig, siginfo_t *info, void *extra)
{
  if (sig != SIGSEGV) return;
  char* buf = "In HANDLER\n";
  int sz = strlen(buf);
  write(STDERR_FILENO, buf, sz);
  //fprintf(stderr, "[Max stack size: %lluMb]\n", limit.rlim_cur / 1024 / 1024);
  _exit(1);
  //raise_exn((uintptr_t)&exn_INTERRUPT);
  return; // never comes here
}
*/

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

  raise_exn((uintptr_t)&exn_INTERRUPT);
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

  raise_exn((uintptr_t)&exn_OVERFLOW);
  return; /* never comes here */
}

#ifndef KAM
extern void code(void);
#endif

#ifndef APACHE

int
main(int argc, char *argv[])
{
  //static struct sigaction sigact;
  //static sigset_t sigset;

  if ((((double)Max_Int) != Max_Int_d) || (((double)Min_Int) != Min_Int_d))
    die("main - integer configuration is erroneous");

  // try to set stack size
  setStackSizeUnlimited();

  parseCmdLineArgs(argc, argv);   /* also initializes ml-access to args */

#ifdef REGION_PAGE_STAT
rpMap = regionPageMapNew();
#endif /* REGION_PAGE_STAT */

#ifdef PROFILING
  resetProfiler();
#endif

  /* setup handlers */
  /*
  if ( sigemptyset(&sigset) == -1 ) {
    die("failed to create empty signal set");
    exit(1);
  }
  if ( sigprocmask(SIG_SETMASK, &sigset, NULL) == -1 ) {
    die("failed to clear signal processing mask");
    exit(1);
  }
  sigact.sa_flags = SA_SIGINFO;
  if ( sigemptyset(&sigact.sa_mask) == -1 ) {
    die("failed to create empty signal mask");
    exit(1);
  }
  sigact.sa_sigaction = sig_handler_segv;
  if ( sigaction(SIGSEGV, &sigact, NULL) == -1 ) {
    die ("failed to set SIGSEGV signal handler");
    exit(1);
  }
  */
  //signal(SIGINT, (SignalHandler)sig_handler_int);
  //signal(SIGFPE, (SignalHandler)sig_handler_fpe);

  debug(printf("Starting execution...\n");)
#ifdef KAM
  return (main_interp(argc, argv));
#else
  code();
  return (EXIT_FAILURE);   /* never comes here (i.e., exits through
                            * terminateML or uncaught_exception) */
#endif
}
#endif
