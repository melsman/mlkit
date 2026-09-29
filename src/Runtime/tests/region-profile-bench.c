/* Isolate page-list traversal from compiler polling and object allocation. */
#include "RegionProfile.h"
#include <assert.h>
#include <stdlib.h>
const uintptr_t mlkit_rp_capable = MLKIT_RP_MAGIC;
Rp *global_freelist;
int main(int argc, char **argv) {
  assert(argc == 3);
  size_t count = strtoull(argv[1],NULL,10);
  assert(count > 0 && count <= 65536);
  Ro r = {0};
  Rp *last = NULL;
  for (size_t i = 0; i < count; i++) {
    Rp *p;
    assert(posix_memalign((void **)&p,sizeof(Rp),sizeof(Rp)) == 0);
    p->n = NULL;
    if (last) last->n = p; else r.g0.fp = p;
    last = p;
  }
  r.g0.a = last->i+100;
  context ctx = {0}; ctx.topregion = &r;
  const uintptr_t end[] = {UINTPTR_MAX,MLKIT_RP_MAGIC};
  uintptr_t base = 0;
  mlkit_rp_enabled = 1; mlkit_rp_interval_us = 0; mlkit_rp_report = 1;
  mlkit_rp_filename = argv[2];
  mlkit_rp_init();
  for (int i = 0; i < 1000; i++) mlkit_rp_capture(&ctx,&base,end+2,2);
  mlkit_rp_close();
  for (Rp *p = r.g0.fp; p;) { Rp *next = p->n; free(p); p = next; }
  return 0;
}
