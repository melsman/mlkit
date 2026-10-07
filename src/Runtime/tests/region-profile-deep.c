/* A valid stack can exceed a million frames. Exercise both validation and
 * recording without making the C process itself recurse. */
#include "RegionProfile.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

const volatile uintptr_t mlkit_rp_capable = MLKIT_RP_MAGIC;
Rp *global_freelist;
static const struct { size_t size; char data[5]; } unit = {0,"deep"};

int main(int argc, char **argv) {
  assert(argc == 2 || argc == 3);
  const size_t depth = 1000017;
  uintptr_t *stack = malloc(depth * sizeof(*stack));
  assert(stack);
  /* Repeated empty frames, each with one return-address word. */
  uintptr_t map[6] = {0,0,0,1,0,MLKIT_RP_MAGIC};
  map[0] = (uintptr_t)&unit - (uintptr_t)&map[0];
  map[1] = (uintptr_t)&unit - (uintptr_t)&map[1];
  uintptr_t end[] = {UINTPTR_MAX,MLKIT_RP_MAGIC};
  for (size_t i = 0; i < depth; i++) stack[i] = (uintptr_t)(map+6);
  stack[depth-1] = (uintptr_t)(end+2);
  if (argc == 3) { assert(!strcmp(argv[2],"cycle")); map[3] = 0; }
  context ctx = {0};
  mlkit_rp_enabled = 1;
  mlkit_rp_interval_us = 0;
  mlkit_rp_filename = argv[1];
  mlkit_rp_init();
  mlkit_rp_capture(&ctx,stack,map+6,2);
  mlkit_rp_close();
  free(stack);
  return 0;
}
