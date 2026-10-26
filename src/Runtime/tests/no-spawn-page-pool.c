/* Correctness probe for the link-selected, parallel-ABI page-pool experiment. */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "Spawn.h"
#include "Exception.h"

/* A C-only probe has no generated ML exception entry; never reach it. */
void raise_exn(Context ctx, uintptr_t exn)
{
  (void)ctx;
  (void)exn;
  abort();
}

void code(Context ctx)
{
  mlkit_regions_may_spawn = 0;
  assert(ctx->freelist == NULL);
  Ro ro;
  Region r = allocateRegion(ctx, &ro, 0);
  size_t after_first = size_free_list();
  uintptr_t *saved[256];
  for (size_t i = 0; i < 256; ++i) {
    saved[i] = alloc(r, 32);
    saved[i][0] = i;
    saved[i][31] = ~i;
  }
  for (size_t i = 0; i < 256; ++i) {
    assert(saved[i][0] == i && saved[i][31] == ~i);
  }
  assert(size_free_list() < after_first);
  resetRegion(r);
  assert(size_free_list() == after_first);
  assert(ctx->freelist == NULL);
  assert(size_thread_free_list() == after_first);
  free_thread_free_list();
  assert(size_free_list() == after_first);

  uintptr_t *large = alloc(r, 4096);
  large[0] = 42;
  large[4095] = 84;
  assert(large[0] + large[4095] == 126);
  resetRegion(r);
  assert(clearStatusBits(r)->lobjs == NULL);
  deallocateRegion(ctx);
  assert(size_free_list() == after_first + 1);
  assert(ctx->freelist == NULL);
  puts("no-spawn page pool passed: growth, reset, deallocation, large objects, cleanup");
  exit(0);
}
