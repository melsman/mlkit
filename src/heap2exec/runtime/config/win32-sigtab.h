/* win32-sigtab.h
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 *
 * fake "signals" to make win32 go through.
 * unlike the unix counterpart, this file is not generated -- do not delete!
 */

#ifndef _WIN32_SIGTAB_
#define _WIN32_SIGTAB_

struct {
  int n;
  char *sname,*lname;
} win32SigTab[] = {
  {0, "INT", "SIGINT"},
  {1, "ALRM", "SIGALRM"},
  {2, "TERM", "SIGTERM"},
  {3, "GC", "RUNSIG_GC"}
};

#define NUM_SIGS 4


#endif

/* end of win32-sigtab.h */
