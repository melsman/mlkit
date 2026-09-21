/* GC frame descriptors immediately precede their saved return PCs. */
#include "Arm64GC.h"
#include <stdio.h>
#include <stdlib.h>

typedef struct Image {
  const void *identity;
  uintptr_t begin, end;
  uintptr_t **roots;
  size_t root_count;
  struct Image *next;
} Image;
static Image *images;

static void invalid(const char *reason) {
  fprintf(stderr, "ARM64 GC metadata: %s\n", reason);
  abort();
}
void mlkit_arm64_register_static_image(const void *identity,
                                      const void *begin, const void *end,
                                      uintptr_t **roots, size_t root_count) {
  for (Image *p=images;p;p=p->next)
    if (p->identity==identity) invalid("image registered twice");
  if ((uintptr_t)begin>(uintptr_t)end)
    invalid("invalid image bounds");
  Image *p=calloc(1,sizeof(*p));
  if (!p) invalid("out of memory");
  p->identity=identity; p->begin=(uintptr_t)begin; p->end=(uintptr_t)end;
  p->roots=roots; p->root_count=root_count; p->next=images; images=p;
}
void mlkit_arm64_unregister_static_image(const void *identity) {
  Image **link=&images;
  while (*link && (*link)->identity!=identity) link=&(*link)->next;
  if (!*link) invalid("unknown image");
  Image *p=*link; *link=p->next; free(p);
}
int mlkit_arm64_static_pointer(const void *ptr) {
  uintptr_t v=(uintptr_t)ptr;
  for(Image *p=images;p;p=p->next) if(v>=p->begin && v<p->end) return 1;
  return 0;
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
  for(Image *p=images;p;p=p->next)
    for(size_t i=0;i<p->root_count;i++) *p->roots[i]=visit(*p->roots[i]);
}
