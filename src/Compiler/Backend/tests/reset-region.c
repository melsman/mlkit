/* Generated helper versus C reset semantics, including real C fallback. */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "Region.h"
#include "Tagging.h"
extern Region generated_reset(Region);
/* Standalone C entry never invokes a collector/ML exception. Satisfy the
 * collector reporting identities normally supplied by generated link code. */
uintptr_t exn_OVERFLOW[2], exn_INTERRUPT[2];
void *data_begin_addr, *data_end_addr;
static size_t fallbacks;
static uintptr_t *pending0;
#ifdef ENABLE_GEN_GC
static uintptr_t *pending1;
#endif
Region counted_reset(Region r) {
  Ro *ro = clearStatusBits(r);
  /* All eligibility checks must precede any writes, including g0 when only
   * g1 has another page. Verify the actual preserving fallback sees old state. */
  assert(ro->g0.a == pending0);
#ifdef ENABLE_GEN_GC
  assert(ro->g1.a == pending1);
#endif
  ++fallbacks;
  return resetRegion(r);
}
static void prepare(Region r, int scenario) {
  Ro *ro = clearStatusBits(r);
  if (scenario == 1) (void)alloc(r, 1);
  if (scenario == 2 || scenario == 3) {
    (void)alloc(r, ALLOCATABLE_WORDS_IN_REGION_PAGE);
    if (scenario == 3) (void)alloc(r, 1);
  }
  if (scenario == 4) {
    size_t n = ALLOCATABLE_WORDS_IN_REGION_PAGE+1;
    uintptr_t *p = alloc(r,n);
#ifdef PROFILING
    p[0] = 0;
    p[1] = val_tag_table(n-2);
#else
    p[0] = val_tag_table(n-1);
#endif
  }
#ifdef ENABLE_GEN_GC
  if (scenario == 5) ro->g1.a = alloc_new_page(&ro->g1);
#else
  (void)ro;
#endif
}
static void verify(Ro *ro, Rp *fp0
#ifdef ENABLE_GEN_GC
                   , Rp *fp1
#endif
                   ) {
  assert(ro->g0.fp == fp0);
  assert(ro->g0.a == clear_fp(fp0)->i);
  assert(clear_fp(fp0)->n == NULL);
  assert(ro->lobjs == NULL);
#ifdef ENABLE_GEN_GC
  assert(ro->g1.fp == fp1);
  assert(ro->g1.a == clear_fp(fp1)->i);
  assert(clear_fp(fp1)->n == NULL);
  assert(clear_fp(fp0)->colorPtr == ro->g0.a);
  assert(clear_fp(fp1)->colorPtr == ro->g1.a);
#endif
}
void code(Context ctx) {
  for (int scenario=0; scenario<6; ++scenario)
    for (uintptr_t bits=0; bits<4; ++bits)
      for (uintptr_t mark=0; mark<32; ++mark) {
        Ro ro, reference;
        Region r = allocateRegion(ctx,&ro,1);
        prepare(r,scenario);
        Region ref = allocateRegion(ctx,&reference,1);
        prepare(ref,scenario);
#if defined(ENABLE_GC) || defined(PROFILING)
        ro.g0.fp = (Rp *)((uintptr_t)clear_fp(ro.g0.fp) | mark);
        reference.g0.fp = (Rp *)((uintptr_t)clear_fp(reference.g0.fp) | mark);
#endif
        Rp *fp0 = ro.g0.fp;
#ifdef ENABLE_GEN_GC
        Rp *fp1 = ro.g1.fp;
#endif
        Region previous = ro.p;
#ifdef PARALLEL
        void *mutex = ro.mutex;
#endif
        pending0 = ro.g0.a;
#ifdef ENABLE_GEN_GC
        pending1 = ro.g1.a;
#endif
        size_t before = fallbacks;
        Region tagged = (Region)((uintptr_t)&ro | bits);
        assert(generated_reset(tagged) == tagged);
#ifdef FAST_RESET
        assert(fallbacks-before == (size_t)(scenario == 3 || scenario == 4
#ifdef ENABLE_GEN_GC
                                           || scenario == 5
#endif
                                           ));
#else
        (void)before;
#endif
        assert(resetRegion(ref) == ref);
        verify(&ro,fp0
#ifdef ENABLE_GEN_GC
               ,fp1
#endif
               );
        verify(&reference,reference.g0.fp
#ifdef ENABLE_GEN_GC
               ,reference.g1.fp
#endif
               );
        assert(ro.p == previous);
#ifdef PARALLEL
        assert(ro.mutex == mutex);
#endif
        deallocateRegion(ctx);
        deallocateRegion(ctx);
      }
  puts("generated/C reset passed");
  exit(0);
}
