#define _POSIX_C_SOURCE 200809L
#include <errno.h>
#include <stdint.h>
#include <time.h>
#include <mach/mach_time.h>
static uint64_t ticks(void) { return mach_absolute_time(); }
static uint64_t duration(void) {
  mach_timebase_info_data_t scale;
  mach_timebase_info(&scale);
  return UINT64_C(250000000)*scale.denom/scale.numer;
}
long tp_measure_busy(long seed) {
  uint64_t end = ticks()+duration();
  volatile uint64_t value = (uint64_t)seed;
  do { for (unsigned i=0;i<1000;i++) value=value*1664525+1013904223; } while(ticks()<end);
  return (long)(value & 1023);
}
long tp_measure_sleep(long unused) {
  (void)unused;
  struct timespec left={0,250000000};
  while(nanosleep(&left,&left) && errno==EINTR) {}
  return 0;
}
