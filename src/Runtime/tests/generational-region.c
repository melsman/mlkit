/* Leaf and fallback region entry/exit must initialise the same two generations. */
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "Region.h"
#include "GC.h"
#include "CommandLine.h"
#include "Exception.h"

extern long rp_used;
static size_t static_data;
size_t *data_begin_addr = &static_data;
size_t *data_end_addr = &static_data;
Exception *exn_INTERRUPT, *exn_OVERFLOW;
void raise_exn(Context ctx, uintptr_t exn) { (void)ctx; (void)exn; abort(); }

typedef Region (*Entry)(Context, Region, Protect);
static void check_region(Region r, uintptr_t kind)
{
  r = clearStatusBits(r);
  Gen *gens[] = {&r->g0, &r->g1};
  assert(!is_gen_1(r->g0) && is_gen_1(r->g1));
  assert(clear_fp(r->g0.fp) != clear_fp(r->g1.fp));
  for (size_t i = 0; i < 2; ++i) {
    Gen *g = gens[i];
    Rp *p = clear_fp(g->fp);
    assert(rtype(*g) == kind && p->gen == g && p->n == NULL);
    assert(g->a == (uintptr_t *)&p->i && p->colorPtr == g->a);
  }
}

void code(Context ctx)
{
  Ro ro;
  rp_gc_treshold = SIZE_MAX;
  time_to_gc = 0;
  /* Warm the freelist, then exercise each typed entry with reusable pages. */
  allocateRegion(ctx, &ro, 0);
  deallocateRegion(ctx);
  Entry entries[] = {allocateRegion, allocatePairRegion, allocateArrayRegion,
                     allocateRefRegion, allocateTripleRegion};
  uintptr_t kinds[] = {0, RTYPE_PAIR, RTYPE_ARRAY, RTYPE_REF, RTYPE_TRIPLE};
  for (size_t i = 0; i < 5; ++i) {
    size_t free_before = size_free_list();
    long used_before = rp_used;
    rp_gc_treshold = (size_t)rp_used + 2;
    Region r = entries[i](ctx, &ro, 0);
    check_region(r, kinds[i]);
    assert(rp_used == used_before + 2 && !time_to_gc);
    assert(size_free_list() == free_before - 2);
    deallocateRegion(ctx);
    assert(rp_used == used_before && size_free_list() == free_before);
    assert(ctx->topregion == NULL);
  }
  /* A pending request must still be recorded while collection is disabled. */
  disable_gc = 1;
  rp_gc_treshold = 0;
  allocateRegion(ctx, &ro, 0);
  check_region(&ro, 0);
  assert(time_to_gc);
  deallocateRegion(ctx);
  rp_gc_treshold = SIZE_MAX;
  /* A pending request does not prevent the safe freelist path. */
  allocateArrayRegion(ctx, &ro, 0);
  check_region(&ro, RTYPE_ARRAY);
  assert(time_to_gc);
  deallocateRegion(ctx);
  time_to_gc = 0;
  disable_gc = 0;

  /* Only one available page: fall back without losing the detached chain. */
  Rp *saved = global_freelist->n;
  global_freelist->n = NULL;
  allocateRegion(ctx, &ro, 0);
  check_region(&ro, 0);
  deallocateRegion(ctx);
  Rp *tail = global_freelist;
  while (tail->n) tail = tail->n;
  tail->n = saved;

  /* Return complete multi-page chains from both generations. */
  allocateRegion(ctx, &ro, 0);
  for (size_t i = 0; i < 64; ++i) {
    allocGen(&ro.g0, 64)[0] = i;
    allocGen(&ro.g1, 64)[0] = i;
  }
  size_t pages = size_free_list();
  deallocateRegion(ctx);
  assert(size_free_list() > pages + 2 && ctx->topregion == NULL);
  puts("generational region entry/exit passed");
  exit(0);
}
