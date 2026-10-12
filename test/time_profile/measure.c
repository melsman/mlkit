#define _POSIX_C_SOURCE 200809L
#include <errno.h>
#include <stdint.h>
#include <time.h>
#ifdef __APPLE__
#include <mach/mach_time.h>
static uint64_t ticks(void) { return mach_absolute_time(); }
static uint64_t duration(void) {
  mach_timebase_info_data_t scale;
  mach_timebase_info(&scale);
  return UINT64_C(250000000)*scale.denom/scale.numer;
}
#else
static uint64_t ticks(void) {
  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC,&now);
  return (uint64_t)now.tv_sec*UINT64_C(1000000000)+(uint64_t)now.tv_nsec;
}
static uint64_t duration(void) { return UINT64_C(250000000); }
#endif
long tp_measure_busy(long seed) {
  uint64_t end = ticks()+duration();
  volatile uint64_t value = (uint64_t)seed;
  do { for (unsigned i=0;i<1000;i++) value=value*1664525+1013904223; } while(ticks()<end);
  return (long)(value & 1023);
}
long tp_measure_sleep(long unused) {
  (void)unused;
#ifdef __APPLE__
  struct timespec left={0,250000000};
  while(nanosleep(&left,&left) && errno==EINTR) {}
#else
  /* A fixed deadline avoids remainder rounding accumulating under 10us signals. */
  uint64_t end = ticks()+duration();
  struct timespec deadline={(time_t)(end/1000000000),(long)(end%1000000000)};
  while(clock_nanosleep(CLOCK_MONOTONIC,TIMER_ABSTIME,&deadline,NULL)==EINTR) {}
#endif
  return 0;
}
