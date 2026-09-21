/* ARM return PCs name executable instructions. Frame descriptors live in
 * immutable image data and are looked up here, never decoded before a PC. */
#include "Arm64GC.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Image {
  const MLKitArm64Frame *identity;
  MLKitArm64Frame *frames;
  size_t count;
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
static int compare(const void *a, const void *b) {
  uintptr_t x=((const MLKitArm64Frame *)a)->pc;
  uintptr_t y=((const MLKitArm64Frame *)b)->pc;
  return (x>y)-(x<y);
}
void mlkit_arm64_register_image(const MLKitArm64Frame *frames, size_t count,
                               const void *begin, const void *end,
                               uintptr_t **roots, size_t root_count) {
  for (Image *p=images;p;p=p->next)
    if (p->identity==frames) invalid("image registered twice");
  if (count>SIZE_MAX/sizeof(*frames) || (uintptr_t)begin>(uintptr_t)end)
    invalid("invalid image bounds");
  Image *p=calloc(1,sizeof(*p));
  if (!p) invalid("out of memory");
  p->frames=malloc((count ? count : 1)*sizeof(*frames));
  if (!p->frames) invalid("out of memory");
  if (count) memcpy(p->frames,frames,count*sizeof(*frames));
  qsort(p->frames,count,sizeof(*frames),compare);
  for(size_t i=0;i<count;i++) {
    if (!p->frames[i].pc || !p->frames[i].anchor ||
        (i && p->frames[i-1].pc==p->frames[i].pc)) invalid("invalid return-PC index");
  }
  p->identity=frames; p->count=count; p->begin=(uintptr_t)begin; p->end=(uintptr_t)end;
  p->roots=roots; p->root_count=root_count; p->next=images; images=p;
}
void mlkit_arm64_unregister_image(const MLKitArm64Frame *frames) {
  Image **link=&images;
  while (*link && (*link)->identity!=frames) link=&(*link)->next;
  if (!*link) invalid("unknown image");
  Image *p=*link; *link=p->next; free(p->frames); free(p);
}
int mlkit_arm64_static_pointer(const void *ptr) {
  uintptr_t v=(uintptr_t)ptr;
  for(Image *p=images;p;p=p->next) if(v>=p->begin && v<p->end) return 1;
  return 0;
}
static const uintptr_t *descriptor(uintptr_t pc) {
  MLKitArm64Frame key={pc,NULL};
  for(Image *p=images;p;p=p->next) {
    MLKitArm64Frame *f=bsearch(&key,p->frames,p->count,sizeof(key),compare);
    if(f) return f->anchor;
  }
  invalid("unregistered return PC");
  return NULL;
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
    const uintptr_t *fd=descriptor(pc);
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
