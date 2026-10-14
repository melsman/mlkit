/* Native clock, interrupted registers and pthread stack discovery. All stack
 * discovery happens before delivery is enabled; only clock/register reads are
 * used by handlers. Linux CLOCK_MONOTONIC excludes suspend, like Mach uptime. */
#if defined(__APPLE__) && defined(__aarch64__)
#include <mach/mach_time.h>
typedef mach_timebase_info_data_t TpTimeScale;
#define TP_CLOCK_DESCRIPTION "mach-uptime-ns (excludes machine sleep)"
static uint64_t tp_clock_tick(void) { return mach_absolute_time(); }
static int tp_clock_init(TpTimeScale *scale) { return mach_timebase_info(scale); }
static uintptr_t tp_context_sp(ucontext_t *uc) { return __darwin_arm_thread_state64_get_sp(uc->uc_mcontext->__ss); }
static uintptr_t tp_context_pc(ucontext_t *uc) { return __darwin_arm_thread_state64_get_pc(uc->uc_mcontext->__ss); }
static int tp_stack_bounds(pthread_t thread, uintptr_t *low, uintptr_t *high) {
  *high = (uintptr_t)pthread_get_stackaddr_np(thread);
  *low = *high-pthread_get_stacksize_np(thread);
  return *low >= *high;
}
#ifdef PARALLEL
static int tp_cond_init(pthread_cond_t *cond) { (void)cond; return 0; }
static int tp_cond_wait_ns(pthread_cond_t *cond, pthread_mutex_t *lock, uint64_t ns) {
  struct timespec delay = {(time_t)(ns/1000000000),(long)(ns%1000000000)};
  return pthread_cond_timedwait_relative_np(cond,lock,&delay);
}
#endif
#elif defined(__linux__) && defined(__x86_64__)
#include <ucontext.h>
typedef struct TpTimeScale { uint32_t numer, denom; } TpTimeScale;
#define TP_CLOCK_DESCRIPTION "monotonic-ns (excludes machine sleep)"
static uint64_t tp_clock_tick(void) {
  struct timespec now;
  if (clock_gettime(CLOCK_MONOTONIC,&now)) return 0;
  return (uint64_t)now.tv_sec*UINT64_C(1000000000)+(uint64_t)now.tv_nsec;
}
static int tp_clock_init(TpTimeScale *scale) {
  scale->numer = scale->denom = 1;
  return tp_clock_tick() == 0;
}
static uintptr_t tp_context_sp(ucontext_t *uc) { return (uintptr_t)uc->uc_mcontext.gregs[REG_RSP]; }
static uintptr_t tp_context_pc(ucontext_t *uc) { return (uintptr_t)uc->uc_mcontext.gregs[REG_RIP]; }
static int tp_stack_bounds(pthread_t thread, uintptr_t *low, uintptr_t *high) {
  pthread_attr_t attr;
  void *base;
  size_t size;
  int rc = pthread_getattr_np(thread,&attr);
  if (rc) return rc;
  rc = pthread_attr_getstack(&attr,&base,&size);
  pthread_attr_destroy(&attr);
  if (rc) return rc;
  *low = (uintptr_t)base; *high = *low+size;
  return *high <= *low;
}
#ifdef PARALLEL
static int tp_cond_init(pthread_cond_t *cond) {
  pthread_condattr_t attr;
  int rc = pthread_condattr_init(&attr);
  if (rc) return rc;
  rc = pthread_condattr_setclock(&attr,CLOCK_MONOTONIC);
  if (!rc) rc = pthread_cond_init(cond,&attr);
  pthread_condattr_destroy(&attr);
  return rc;
}
static int tp_cond_wait_ns(pthread_cond_t *cond, pthread_mutex_t *lock, uint64_t ns) {
  uint64_t due = tp_clock_tick()+ns;
  struct timespec deadline = {(time_t)(due/1000000000),(long)(due%1000000000)};
  return pthread_cond_timedwait(cond,lock,&deadline);
}
#endif
#endif
