#include <stdio.h>
#include <time.h>
#include <sys/types.h>
#include <sys/resource.h>
extern ssize_t num_gc, time_gc_all_ms;
long msort_mark(long stage) {
  struct timespec t;
  struct rusage u;
  clock_gettime(CLOCK_MONOTONIC, &t);
  getrusage(RUSAGE_SELF, &u);
  fprintf(stderr,"PHASE %ld %.9f %zd %zd %ld\n", stage,
          (double)t.tv_sec+(double)t.tv_nsec/1e9, num_gc,time_gc_all_ms,u.ru_maxrss);
  return 0;
}
