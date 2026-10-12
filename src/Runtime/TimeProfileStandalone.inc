/* Time sampling in the ordinary runtime: no region maps or safe-point polls. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include <time.h>
#include <sys/time.h>
#ifdef __APPLE__
#include <mach-o/dyld.h>
#include <mach-o/loader.h>
#endif
#if defined(__APPLE__) && defined(__aarch64__) && !defined(ARGOBOTS)
#include <mach/mach_time.h>
#include <pthread.h>
#include <sched.h>
#define MLKIT_TP_NATIVE 1
#endif
int mlkit_rp_enabled;
_Atomic int mlkit_rp_pending;
uint64_t mlkit_rp_interval_us, mlkit_rp_interval_entries;
const char *mlkit_p_filename = "profile.prof";
static FILE *output;
static int timer_installed;
static void fail(const char *s) {
  fprintf(stderr,"time profiler: %s\n",s);
  _Exit(EXIT_FAILURE);
}
#ifdef MLKIT_TP_NATIVE
static uint64_t tp_timestamp(void);
#endif
#include "ProfileOutput.inc"
void mlkit_rp_close(void) {
  if (!output) return;
  tp_shutdown();
  uint64_t end = 0;
#ifdef MLKIT_TP_NATIVE
  end = tp_timestamp();
#endif
  emit_record(4,NUMS(end,0,0,0),NO_STRINGS);
  if (fclose(output)) fail("cannot close profile output");
  output = NULL;
}
void mlkit_rp_init(void) {
  if (!mlkit_tp_enabled) return;
  tp_prepare();

#ifndef PARALLEL
  struct sigaction previous, action = {0};
  struct itimerval old;
  if (getitimer(ITIMER_REAL,&old) || sigaction(SIGALRM,NULL,&previous))
    fail("cannot inspect sampling timer");
  if (old.it_value.tv_sec || old.it_value.tv_usec || previous.sa_handler != SIG_DFL)
    fail("SIGALRM/ITIMER_REAL already in use; disable -tp or release the conflicting timer/handler");

#endif
  output = fopen(mlkit_p_filename,"wb");
  if (!output) fail("cannot open profile output");
  const unsigned char magic[] = {'M','L','K','R','P',0,10,0};
  if (fwrite(magic,1,sizeof(magic),output) != sizeof(magic)) fail("cannot write profile header");
#ifdef ENABLE_GC
  const unsigned gc = 1;
#else
  const unsigned gc = 0;
#endif
  emit_record(1,NUMS(sizeof(uintptr_t),sizeof(Rp),gc),STRS("time profile"));
  emit_code_metadata();
  tp_begin();
  if (fflush(output)) fail("cannot write profile header");

#ifndef PARALLEL
  tp_install_handler(&action);
  sigemptyset(&action.sa_mask);
  if (sigaction(SIGALRM,&action,NULL)) fail("cannot install sampling handler");
  timer_installed = 1;
#ifdef MLKIT_TP_NATIVE
  tp_alarm();
#endif

#endif
  if (atexit(mlkit_rp_close)) fail("cannot register output cleanup");
}
