/* Inspect the link policy and real descriptors, including startup globals. */
#include <assert.h>
#include <stdint.h>
#include "Region.h"

void mlkit_spawn_unit_keep(void) { }

void mlkit_spawn_policy(Context ctx, uintptr_t expected) {
  assert(mlkit_regions_may_spawn == expected);
  size_t protected = 0, total = 0;
  for (Region r = ctx->topregion; r; r = r->p) {
    ++total;
    if (r->mutex) ++protected;
    if (!expected) assert(r->mutex == NULL);
  }
  assert(total >= 6);
  if (expected) assert(protected >= 6);
  Region top = ctx->topregion;
  for (int request = 0; request <= 1; ++request) {
    Ro descriptor;
    Region r = allocateRegion(ctx, &descriptor, request);
    assert((descriptor.mutex != NULL) == (request && expected));
    uintptr_t *first = alloc(r, ALLOCATABLE_WORDS_IN_REGION_PAGE);
    first[0] = 123;
    uintptr_t *next = alloc(r, 1);
    assert(next != first + ALLOCATABLE_WORDS_IN_REGION_PAGE);
    uintptr_t *large = alloc(r, 2048);
    large[2047] = 456;
    assert(first[0] == 123 && large[2047] == 456);
    resetRegion(r);
    assert(descriptor.lobjs == NULL);
    deallocateRegion(ctx);
    assert(ctx->topregion == top);
  }
}
