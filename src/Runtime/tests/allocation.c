/* C-only smoke test: no claim about generated ML code or GC stack maps. */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "Region.h"

void code(Context ctx)
{
  Ro ro;
  Region r = allocateRegion(ctx, &ro, 0);
  uintptr_t *saved[4096];
  for (size_t i = 0; i < 4096; ++i) {
    saved[i] = alloc(r, 32);
    saved[i][0] = i;
    saved[i][31] = ~i;
  }
  for (size_t i = 0; i < 4096; ++i) {
    assert(saved[i][0] == i);
    assert(saved[i][31] == ~i);
  }
  uintptr_t *large = alloc(r, 4096);
  large[0] = 42;
  large[4095] = 84;
  assert(large[0] + large[4095] == 126);
  deallocateRegion(ctx);
  assert(ctx->topregion == NULL);
  puts("runtime allocation smoke test passed");
  exit(0);
}
