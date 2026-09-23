/* GC frame descriptors immediately precede their saved return PCs. */
#include "Arm64GC.h"
#include <stdio.h>
#include <stdlib.h>

static void invalid(const char *reason) {
  fprintf(stderr, "ARM64 GC metadata: %s\n", reason);
  abort();
}

/* Keep the entry points used by previously compiled ARM units. */
void mlkit_arm64_register_static_image(const void *identity, const void *begin,
                                      const void *end, uintptr_t **roots, size_t count) {
  mlkit_gc_register_static_image(identity, begin, end, roots, count);
}
void mlkit_arm64_seal_main_image(void) { mlkit_gc_seal_main_image(); }
void mlkit_arm64_unregister_static_image(const void *identity) {
  mlkit_gc_unregister_static_image(identity);
}
int mlkit_arm64_static_pointer(const void *ptr) {
  return mlkit_gc_static_pointer(ptr);
}
static size_t even(size_t n) { return n+(n&1); }
void mlkit_arm64_visit_roots(uintptr_t *snapshot, uintptr_t mask,
                           uintptr_t (*visit)(uintptr_t)) {
  /* Slots 31-i hold xi; slot zero is the entry SP, not a root. */
  if(mask & ~UINT64_C(0x07f8ffff)) invalid("reserved register marked as root");
  for(unsigned i=0;i<32;i++) if(mask & ((uintptr_t)1<<i))
    snapshot[31-i]=visit(snapshot[31-i]);
  size_t skipped=snapshot[40], results=snapshot[41], args=snapshot[42];
  uintptr_t *incoming=(uintptr_t *)snapshot[43];
  if(skipped>args) invalid("invalid spilled-argument counts");
  for(size_t i=skipped;i<args;i++) incoming[i]=visit(incoming[i]);
  uintptr_t pc=incoming[even(args)+1];
  uintptr_t *base=incoming+even(args)+2+even(results);
  for(;;) {
    const uintptr_t *fd=(const uintptr_t *)pc;
    size_t words=fd[-3], ret=fd[-2];
    if(words==UINTPTR_MAX) break;
    if(ret>=words) invalid("return slot outside frame");
    for(size_t bit=0;bit<words;bit++)
      if(fd[-4-(ptrdiff_t)(bit/32)] & ((uintptr_t)1<<(bit%32)))
        base[words-1-bit]=visit(base[words-1-bit]);
    pc=base[ret]; base+=words;
  }
  mlkit_gc_visit_static_roots(visit);
}
