/* Execute generated fast/slow paths against real runtime region descriptors. */
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include "Region.h"
#include "Tagging.h"
#ifdef ENABLE_GC
#include "GC.h"
#include "CommandLine.h"
#endif

static Ro region;
static uintptr_t finite[8];
static uintptr_t *before;
#ifdef ENABLE_GC
static unsigned long allocated_before;

/* Both allocation paths must retain requests during a foreign-call extent.
 * No collection may happen here, and the caller's deferral state is unchanged. */
static void check_deferred_requests(Context ctx) {
  Ro test_region;
  long saved_disable = disable_gc;
  size_t saved_request = time_to_gc;
  size_t saved_pages = rp_gc_treshold;
  size_t saved_large = lobjs_gc_treshold;
  disable_gc = 1;
  allocateRegion(ctx, &test_region, 0);
  time_to_gc = 0;
  rp_gc_treshold = 0;
  alloc_new_page(&test_region.g0);
  assert(time_to_gc == 1 && disable_gc == 1);
  time_to_gc = 0;
  lobjs_gc_treshold = 0;
  *alloc(&test_region, 2048) = val_tag_table(2047);
  assert(time_to_gc == 1 && disable_gc == 1);
  /* A disabled collector must not inspect even an invalid root image. */
  gc(ctx, NULL, 0);
  assert(time_to_gc == 1 && disable_gc == 1);
  deallocateRegion(ctx);
  rp_gc_treshold = saved_pages;
  lobjs_gc_treshold = saved_large;
  time_to_gc = saved_request;
  disable_gc = saved_disable;
}
#endif

/* Entry/exit preserve page chains and descriptors on both leaf and slow
 * paths. Keep these freelist manipulations out of parallel/generational tests. */
#if !defined(PARALLEL) && !defined(ENABLE_GEN_GC)
static void check_region_calls(Context ctx) {
  Ro outer, inner;
  Region previous = ctx->topregion;
#ifdef ENABLE_GC
  extern long rp_used;
  size_t saved_threshold = rp_gc_treshold, saved_request = time_to_gc;
  long saved_disable = disable_gc;
  rp_gc_treshold = (size_t)-1;
  disable_gc = 1;
  time_to_gc = 0;
#endif
  /* Force the empty-list fallback, then restore the detached free pages. */
  Rp *saved_free = FREELIST;
  FREELIST = NULL;
  Region result = allocateRegion(ctx, &outer, 0);
  assert(clearStatusBits(result) == &outer && ctx->topregion == &outer);
  assert(outer.p == previous && outer.lobjs == NULL);
  assert(clear_fp(outer.g0.fp)->gen == &outer.g0);
  Rp *tail = FREELIST;
  if (tail) {
    while (tail->n) tail = tail->n;
    tail->n = saved_free;
  } else FREELIST = saved_free;
  size_t pages = size_free_list();
  allocateRegion(ctx, &inner, 0);
  assert(inner.p == &outer && ctx->topregion == &inner);
  assert(size_free_list() == pages - 1);
  inner.g0.a = alloc_new_page(&inner.g0);
  assert(size_free_list() == pages - 2);
  deallocateRegion(ctx);
  assert(ctx->topregion == &outer && size_free_list() == pages);
  /* A large object forces the full deallocation path. */
  *alloc(&outer, 2048) = val_tag_table(2047);
  assert(outer.lobjs != NULL);
  deallocateRegion(ctx);
  assert(ctx->topregion == previous);
#ifdef ENABLE_GC
  Region (*entries[])(Context, Region, Protect) = {
    allocatePairRegion, allocateArrayRegion, allocateRefRegion, allocateTripleRegion
  };
  unsigned kinds[] = {RTYPE_PAIR, RTYPE_ARRAY, RTYPE_REF, RTYPE_TRIPLE};
  for (unsigned i = 0; i < 4; i++) {
    long used = rp_used;
    entries[i](ctx, &inner, 0);
    assert(rp_used == used + 1);
    assert(rtype(inner.g0) == kinds[i]);
    assert(clear_fp(inner.g0.fp)->gen == &inner.g0);
    deallocateRegion(ctx);
    assert(ctx->topregion == previous && rp_used == used);
  }
  /* Crossing the threshold must request GC despite foreign-call deferral. */
  rp_gc_treshold = 0;
  time_to_gc = 0;
  allocateRegion(ctx, &inner, 0);
  assert(time_to_gc && disable_gc == 1);
  deallocateRegion(ctx);
  /* An already pending request survives another entry and its page count. */
  long used = rp_used;
  allocateRegion(ctx, &inner, 0);
  assert(time_to_gc && rp_used == used + 1 && disable_gc == 1);
  deallocateRegion(ctx);
  assert(rp_used == used);
  /* Collector-time allocation must retain the slow path's to-space mark. */
  size_t saved_doing_gc = doing_gc;
  doing_gc = 1;
  allocateRegion(ctx, &inner, 0);
  assert(is_tospace_bit(clear_fp(inner.g0.fp)->n));
  deallocateRegion(ctx);
  doing_gc = saved_doing_gc;
  rp_gc_treshold = saved_threshold;
  time_to_gc = saved_request;
  disable_gc = saved_disable;
#endif
}
#endif

uintptr_t allocation_prepare(Context ctx, uintptr_t id) {
#if !defined(PARALLEL) && !defined(ENABLE_GEN_GC)
  if (id == 0) check_region_calls(ctx);
#endif
#ifdef ENABLE_GC
  if (id == 1) check_deferred_requests(ctx);
#endif
  if (id == 4 || id == 7 || id == 17) return (uintptr_t)finite;
#ifdef ENABLE_GC
  if (id == 14) allocatePairRegion(ctx, &region, 0);
  else allocateRegion(ctx, &region, 0);
#else
  allocateRegion(ctx, &region,
#ifdef PARALLEL
                 id != 0 && id != 8 && id != 9
#else
                 0
#endif
                 );
#endif
  if (id == 1) alloc(&region, ALLOCATABLE_WORDS_IN_REGION_PAGE - 2);
  if (id == 8) alloc(&region, ALLOCATABLE_WORDS_IN_REGION_PAGE - 3);
  if (id == 2 || id == 9) alloc(&region, ALLOCATABLE_WORDS_IN_REGION_PAGE);
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
                      uintptr_t d15, uintptr_t d29, uintptr_t r1, uintptr_t r2, uintptr_t r3) {
  assert(r0 == 100 && r4 == 101 && r8 == 102 && r15 == 103);
  assert(r21 == 104 && r26 == 105 && d0 == 106 && d7 == 107 && d15 == 108 && d29 == 109);
  assert(r1 == 110 && r2 == 111 && r3 == 112);
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
    if (id == 2 || id == 9) {
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
