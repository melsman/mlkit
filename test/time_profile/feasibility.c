/* T1 only: single-thread Darwin ARM64 interrupted-PC experiment. */
#include <errno.h>
#include <dlfcn.h>
#include <inttypes.h>
#include <signal.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#include <mach/mach_time.h>
#include <pthread.h>
#if !defined(__APPLE__) || !defined(__aarch64__)
#error This experiment requires macOS ARM64
#endif
#define CAPACITY 65536
_Static_assert(ATOMIC_LONG_LOCK_FREE == 2 && ATOMIC_INT_LOCK_FREE == 2,
               "Handler needs always lock-free atomics");
struct sample { _Atomic uintptr_t pc; _Atomic uint64_t tick; _Atomic int main_thread; };
static struct sample samples[CAPACITY];
static _Atomic int used, dropped;
static int timer_kind, timer_signal;
static sigset_t mask;
static mach_timebase_info_data_t scale;
static uint64_t start_tick;
static struct rusage start_usage;
static unsigned interval;
static uintptr_t stack_low, stack_high;
static void die(const char *s) { perror(s); exit(1); }
static double seconds(uint64_t ticks) {
  return (double)ticks * scale.numer / scale.denom / 1e9;
}
static void capture(int sig, siginfo_t *info, void *context) {
  int saved_errno = errno;
  (void)sig; (void)info;
  int n = atomic_load_explicit(&used, memory_order_relaxed);
  if (n < CAPACITY) {
    ucontext_t *uc = context;
    atomic_store_explicit(&samples[n].pc,
      (uintptr_t)__darwin_arm_thread_state64_get_pc(uc->uc_mcontext->__ss), memory_order_relaxed);
    atomic_store_explicit(&samples[n].tick, mach_absolute_time(), memory_order_relaxed);
    uintptr_t sp = (uintptr_t)__darwin_arm_thread_state64_get_sp(uc->uc_mcontext->__ss);
    atomic_store_explicit(&samples[n].main_thread,
      sp >= stack_low && sp < stack_high, memory_order_relaxed);
    atomic_store_explicit(&used, n + 1, memory_order_release);
  } else if (dropped < INT32_MAX) dropped++;
  errno = saved_errno;
}
uintptr_t tp_start(void) {
  const char *mode = getenv("TP_MODE");
  const char *value = getenv("TP_INTERVAL_US");
  interval = value ? (unsigned)strtoul(value, NULL, 10) : 1000;
  if (!interval || interval > 1000000) { fprintf(stderr,"bad interval\n"); exit(1); }
  if (!mode || !strcmp(mode,"wall")) { timer_kind = ITIMER_REAL; timer_signal = SIGALRM; }
  else if (!strcmp(mode,"user")) { timer_kind = ITIMER_VIRTUAL; timer_signal = SIGVTALRM; }
  else if (!strcmp(mode,"cpu")) { timer_kind = ITIMER_PROF; timer_signal = SIGPROF; }
  else { fprintf(stderr,"bad mode\n"); exit(1); }
  struct itimerval old;
  struct sigaction previous;
  if (getitimer(timer_kind,&old) || sigaction(timer_signal,NULL,&previous)) die("inspect timer");
  if (old.it_value.tv_sec || old.it_value.tv_usec || previous.sa_handler != SIG_DFL) {
    fprintf(stderr,"timer/signal already owned\n"); exit(1);
  }
  sigemptyset(&mask); sigaddset(&mask,timer_signal);
  if (sigprocmask(SIG_BLOCK,&mask,NULL)) die("block");
  used = dropped = 0;
  mach_timebase_info(&scale);
  errno = 0; /* Resolve the errno accessor before handler use. */
  (void)mach_absolute_time(); /* Resolve timestamp stub before handler use. */
  stack_high = (uintptr_t)pthread_get_stackaddr_np(pthread_self());
  stack_low = stack_high - pthread_get_stacksize_np(pthread_self());
  struct sigaction action = {0};
  action.sa_sigaction = capture; action.sa_flags = SA_SIGINFO;
  action.sa_mask = mask;
  if (sigaction(timer_signal,&action,NULL)) die("sigaction");
  start_tick = mach_absolute_time();
  getrusage(RUSAGE_SELF,&start_usage);
  struct itimerval timer = {0};
  timer.it_value.tv_sec = interval / 1000000;
  timer.it_value.tv_usec = interval % 1000000;
  timer.it_interval = timer.it_value;
  if (setitimer(timer_kind,&timer,NULL)) die("setitimer");
  if (sigprocmask(SIG_UNBLOCK,&mask,NULL)) die("unblock");
  return 0;
}
uintptr_t tp_stop(void) {
  if (sigprocmask(SIG_BLOCK,&mask,NULL)) die("block");
  struct itimerval timer = {0};
  if (setitimer(timer_kind,&timer,NULL)) die("disarm");
  uint64_t end = mach_absolute_time();
  struct rusage usage;
  getrusage(RUSAGE_SELF,&usage);
  double user = usage.ru_utime.tv_sec - start_usage.ru_utime.tv_sec +
    (usage.ru_utime.tv_usec - start_usage.ru_utime.tv_usec)/1e6;
  double system = usage.ru_stime.tv_sec - start_usage.ru_stime.tv_sec +
    (usage.ru_stime.tv_usec - start_usage.ru_stime.tv_usec)/1e6;
  double min = 1e9, max = 0;
  int wrong_thread = 0, zero_pc = 0, backwards = 0;
  for (sig_atomic_t i = 0; i < used; i++) {
    wrong_thread += !samples[i].main_thread;
    zero_pc += !samples[i].pc;
    if (i) {
      backwards += samples[i].tick < samples[i-1].tick;
      double gap = seconds(samples[i].tick - samples[i-1].tick);
      if (gap < min) min = gap;
      if (gap > max) max = gap;
    }
  }
  printf("mode=%s interval_us=%u samples=%d dropped=%d wall=%.6f user=%.6f system=%.6f min_gap=%.6f max_gap=%.6f wrong_thread=%d zero_pc=%d backwards=%d\n",
    getenv("TP_MODE"),interval,used,dropped,seconds(end-start_tick),user,system,
    used > 1 ? min : 0,max,wrong_thread,zero_pc,backwards);
  const char *path = getenv("TP_SAMPLES");
  if (path) {
    FILE *f = fopen(path,"w"); if (!f) die("samples");
    for (sig_atomic_t i = 0; i < used; i++) {
      Dl_info image = {0};
      dladdr((void *)samples[i].pc,&image);
      fprintf(f,"%.9f 0x%" PRIxPTR " 0x%" PRIxPTR " %s %s\n",
        seconds(samples[i].tick-start_tick),samples[i].pc,
        (uintptr_t)image.dli_fbase,image.dli_fname ? image.dli_fname : "unknown",
        image.dli_sname ? image.dli_sname : "unknown");
    }
    fclose(f);
  }
  /* Leave signal blocked: pending expirations must not hit a default handler. */
  if (wrong_thread || zero_pc || backwards) exit(1);
  return 0;
}
uintptr_t tp_busy(void) {
  uint64_t end = mach_absolute_time();
  volatile uint64_t x = 1;
  while (seconds(mach_absolute_time()-end) < 0.3)
    for (int i = 0; i < 10000; i++) x = x*1664525+1013904223;
  return 0;
}
uintptr_t tp_sleep(void) {
  struct timespec remaining = {0,300000000};
  while (nanosleep(&remaining,&remaining)) if (errno != EINTR) die("nanosleep");
  return 0;
}
#ifndef TP_LIBRARY
int main(int argc, char **argv) {
  if (argc != 2) return 2;
  tp_start();
  if (!strcmp(argv[1],"busy")) tp_busy();
  else if (!strcmp(argv[1],"sleep")) tp_sleep();
  else if (!strcmp(argv[1],"blocked")) {
    sigprocmask(SIG_BLOCK,&mask,NULL); tp_busy();
    sigprocmask(SIG_UNBLOCK,&mask,NULL);
  } else if (!strcmp(argv[1],"read")) {
    int fd[2];
    if (pipe(fd)) die("pipe");
    pid_t child = fork();
    if (child < 0) die("fork");
    if (!child) {
      close(fd[0]); tp_sleep();
      if (write(fd[1],"x",1) != 1) _exit(1);
      _exit(0);
    }
    close(fd[1]);
    char byte;
    ssize_t result;
    do { result = read(fd[0],&byte,1); } while (result < 0 && errno == EINTR);
    if (result != 1) die("read");
    close(fd[0]);
    int status;
    while (waitpid(child,&status,0) < 0) if (errno != EINTR) die("waitpid");
    if (!WIFEXITED(status) || WEXITSTATUS(status)) return 1;
  } else if (!strcmp(argv[1],"system")) {
    uint64_t start = mach_absolute_time();
    while (seconds(mach_absolute_time()-start) < 0.3) (void)getppid();
  } else return 2;
  tp_stop();
  return 0;
}
#endif
