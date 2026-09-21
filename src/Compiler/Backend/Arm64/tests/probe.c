/* Native execution probes for the untagged, non-parallel runtime. */
#include <stdint.h>
#include <stdlib.h>

uint64_t arm64_large(const uint64_t *p, uint64_t w) {
  uintptr_t sp;
  __asm__ volatile("mov %0, sp" : "=r"(sp));
  if ((sp & 15) || (uintptr_t)p <= sp || (uintptr_t)p-sp > 65536) abort();
  if (p[0] != w) abort();
  for (unsigned i=1; i<4200; i++) if (p[i] != i) abort();
  return w+1;
}
/* Region.h's plain Gen/Ro/context prefix. Count the live region chain to
 * check that unwinding restores it, rather than merely restoring SP. */
struct region { void *a, *fp; struct region *prev; void *large_objects; };
struct context { struct region *topregion; void *exnptr; };
uint64_t arm64_region_depth(const struct context *ctx) {
  uint64_t n=0;
  for (struct region *r=ctx->topregion; r; r=r->prev)
    if (++n > 10000) abort();
  return n;
}
