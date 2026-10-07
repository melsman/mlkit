/* Linker metadata may be absent or contain multiple entries. Compile this
 * separately from RegionProfile.c at -O2 to exercise weak-symbol replacement. */
#include "RegionProfile.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

const volatile uintptr_t mlkit_rp_capable = MLKIT_RP_MAGIC;
const volatile uintptr_t mlkit_rp_allocation_capable = 4;
Rp *global_freelist;
static Ro regions[2];
#ifdef LINKED_METADATA
const char *const volatile mlkit_rp_ir_objects[][2] = {
  {"first", "first.o"}, {"second", "second.o"}, {NULL, NULL}
};
static Region slots[] = {&regions[0], &regions[1]};
const volatile struct { Region *slot; uintptr_t type; } mlkit_rp_globals[] = {
  {&slots[0], 1}, {&slots[1], 2}, {NULL, 0}
};
#endif

int main(int argc, char **argv) {
  assert(argc == 2);
  mlkit_rp_enabled = 1;
  mlkit_rp_interval_us = 0;
  mlkit_rp_filename = argv[1];
  mlkit_rp_init();
  Rp *pages[2];
  for (size_t i = 0; i < 2; i++) {
    assert(posix_memalign((void **)&pages[i], sizeof(Rp), sizeof(Rp)) == 0);
    memset(pages[i], 0, sizeof(Rp));
    regions[i].g0.fp = pages[i];
    regions[i].g0.a = pages[i]->i;
    mlkit_rp_page_alloc();
  }
  regions[1].p = &regions[0];
  context ctx = {0};
  ctx.topregion = &regions[1];
  uintptr_t stack[1] = {0};
  const uintptr_t sentinel[] = {UINTPTR_MAX, MLKIT_RP_MAGIC};
  mlkit_rp_capture(&ctx, stack, sentinel + 2, 2);
  mlkit_rp_close();
  free(pages[0]);
  free(pages[1]);
  return 0;
}
