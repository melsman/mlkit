/* Manual-only shared-helper benchmark; no sentinel overhead in timed bridge. */
#define _POSIX_C_SOURCE 200809L
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "Region.h"
#include "Tagging.h"
extern Region generated_reset(Region);
uintptr_t exn_OVERFLOW[2], exn_INTERRUPT[2];
void *data_begin_addr, *data_end_addr;
static void large(Region r) {
  size_t n = ALLOCATABLE_WORDS_IN_REGION_PAGE+1;
  uintptr_t *p = alloc(r,n);
#ifdef PROFILING
  p[0]=0; p[1]=val_tag_table(n-2);
#else
  p[0]=val_tag_table(n-1);
#endif
}
void code(Context ctx) {
  const char *scenario = getenv("RESET_SCENARIO");
  if (!scenario) scenario="single";
  int multi=!strcmp(scenario,"multi"), big=!strcmp(scenario,"large");
  int mixed=!strcmp(scenario,"mixed");
  size_t count=(multi || big || mixed) ? 2000000 : 20000000;
  Ro ro;
  Region r=allocateRegion(ctx,&ro,1);
  struct timespec begin,end;
  clock_gettime(CLOCK_MONOTONIC,&begin);
  for (size_t i=0;i<count;i++) {
    if (big) large(r);
    else if (multi || (mixed && i%10==0)) {
      (void)alloc(r,ALLOCATABLE_WORDS_IN_REGION_PAGE);
      (void)alloc(r,1);
    } else ro.g0.a=clear_fp(ro.g0.fp)->i+1;
    assert(generated_reset(r)==r);
  }
  clock_gettime(CLOCK_MONOTONIC,&end);
  assert(ro.g0.a==clear_fp(ro.g0.fp)->i && ro.lobjs==NULL);
  fprintf(stderr,"%.9f\n",(double)(end.tv_sec-begin.tv_sec)+(end.tv_nsec-begin.tv_nsec)/1e9);
  deallocateRegion(ctx);
  puts("reset benchmark passed");
  exit(0);
}
