/* Execute generated fast/slow paths against real runtime region descriptors. */
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include "Region.h"
#include "Tagging.h"
#ifdef ENABLE_GC
#include "GC.h"
#endif

static Ro region;
static uintptr_t finite[8];
static uintptr_t *before;
#ifdef ENABLE_GC
static unsigned long allocated_before;
#endif

uintptr_t allocation_prepare(Context ctx, uintptr_t id) {
  if (id == 4 || id == 7 || id == 17) return (uintptr_t)finite;
#ifdef ENABLE_GC
  if (id == 14) allocatePairRegion(ctx, &region, 0);
  else allocateRegion(ctx, &region, 0);
#else
  allocateRegion(ctx, &region, 0);
#endif
  if (id == 1) alloc(&region, ALLOCATABLE_WORDS_IN_REGION_PAGE - 2);
  if (id == 2) alloc(&region, ALLOCATABLE_WORDS_IN_REGION_PAGE);
  if (id >= 10 || id == 5) alloc(&region, 3);
  if (id == 11) alloc(&region, ALLOCATABLE_WORDS_IN_REGION_PAGE);
  /* GC needs a valid size tag even when a large payload is never read. */
  if (id == 12) *alloc(&region, 2048) = val_tag_table(2047);
#ifdef ENABLE_GEN_GC
  if (id == 13) {
    allocGen(&region.g1, ALLOCATABLE_WORDS_IN_REGION_PAGE);
    allocGen(&region.g1, 1);
  }
  if (id == 14) {
    allocGen(&region.g1, 3);
  }
#endif
  before = region.g0.a;
#ifdef ENABLE_GC
  allocated_before = alloc_period;
#endif
  return (uintptr_t)&region | ((id == 5 || id == 16) ? 3 : 1);
}

void allocation_check(Context ctx, uintptr_t tagged_region, uintptr_t result, uintptr_t id,
                      uintptr_t r0, uintptr_t r4, uintptr_t r8, uintptr_t r15,
                      uintptr_t r21, uintptr_t r26, uintptr_t d0, uintptr_t d7,
                      uintptr_t d15, uintptr_t d29) {
  assert(r0 == 100 && r4 == 101 && r8 == 102 && r15 == 103);
  assert(r21 == 104 && r26 == 105 && d0 == 106 && d7 == 107 && d15 == 108 && d29 == 109);
  if (id == 4 || id == 7 || id == 17) {
    assert(tagged_region == (uintptr_t)finite);
    if (id != 17) assert(result == (uintptr_t)finite);
    return;
  }
  assert(clearStatusBits((Region)tagged_region) == &region);
  if (id >= 10 && id != 15) {
    assert(region.g0.a == clear_fp(region.g0.fp)->i);
    assert(clear_fp(region.g0.fp)->n == NULL && region.lobjs == NULL);
#ifdef ENABLE_GEN_GC
    assert(region.g1.a == clear_fp(region.g1.fp)->i);
    assert(clear_fp(region.g1.fp)->n == NULL);
    assert(clear_fp(region.g0.fp)->colorPtr == region.g0.a);
    assert(clear_fp(region.g1.fp)->colorPtr == region.g1.a);
    if (id == 14) assert(is_pairregion(region.g0) && is_pairregion(region.g1) && is_gen_1(region.g1));
#endif
  } else if (id == 15) {
    assert(region.g0.a == before);  /* At-top dynamic reset is a no-op. */
  } else if (id == 3) {
    assert(region.lobjs != NULL && result == (uintptr_t)&region.lobjs->value);
    assert(region.g0.a == before);
    *(uintptr_t *)result = val_tag_table(2047);
  } else {
    size_t words = id == 1 ? 2 : id == 2 ? 1 : 3;
#ifndef TAG_VALUES
    if (id == 6) words = 4;
#endif
    uintptr_t *expected = id == 5 ? clear_fp(region.g0.fp)->i : before;
    if (id == 2) {
      assert(clear_fp(region.g0.fp)->n != NULL);
      expected = clear_fp(region.g0.fp)->n->i;
    }
#ifdef TAG_VALUES
    assert(result == (uintptr_t)expected - (id == 6 ? 8 : 0));
#else
    assert(result == (uintptr_t)expected);
#endif
    assert(region.g0.a == expected + words);
#ifdef ENABLE_GC
    assert(alloc_period == allocated_before + 8 * words);
#endif
  }
  deallocateRegion(ctx);
}
