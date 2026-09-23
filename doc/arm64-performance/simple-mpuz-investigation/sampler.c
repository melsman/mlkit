#include <signal.h>
#include <sys/time.h>
#include <sys/ucontext.h>
#include <mach-o/dyld.h>
#include <mach/arm/thread_status.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
static uintptr_t pcs[100000];
static volatile sig_atomic_t count;
static intptr_t slide;
static void tick(int signo, siginfo_t *info, void *context) {
  (void)signo; (void)info;
  ucontext_t *u = context;
  if (count < 100000) pcs[count++] = arm_thread_state64_get_pc(u->uc_mcontext->__ss) - slide;
}
static void finish(void) {
  struct itimerval off = {0};
  setitimer(ITIMER_PROF, &off, NULL);
  for (sig_atomic_t i = 0; i < count; ++i) fprintf(stderr, "PC %lx\n", (unsigned long)pcs[i]);
}
__attribute__((constructor)) static void start(void) {
  struct sigaction sa = {0};
  sa.sa_sigaction = tick;
  sa.sa_flags = SA_SIGINFO;
  sigemptyset(&sa.sa_mask);
  if (sigaction(SIGPROF, &sa, NULL)) abort();
  slide = _dyld_get_image_vmaddr_slide(0);
  atexit(finish);
  struct itimerval timer = {{0,1000},{0,1000}};
  if (setitimer(ITIMER_PROF, &timer, NULL)) abort();
}
