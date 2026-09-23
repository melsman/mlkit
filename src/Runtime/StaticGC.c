/* Static images and global root slots shared by native backends. */
#include "StaticGC.h"
#include <stdio.h>
#include <stdlib.h>

typedef struct Image {
  const void *identity;
  uintptr_t begin, end;
  uintptr_t **roots;
  size_t root_count;
  struct Image *next;
} Image;
static Image *images, *main_images;
static int main_sealed;
uintptr_t mlkit_gc_main_begin, mlkit_gc_main_end;
size_t mlkit_gc_dynamic_images;

static void main_bounds(void) {
  mlkit_gc_main_begin = UINTPTR_MAX;
  mlkit_gc_main_end = 0;
  for (Image *p = main_images; p; p = p->next) {
    if (p->begin == p->end) continue;
    if (p->begin < mlkit_gc_main_begin) mlkit_gc_main_begin = p->begin;
    if (p->end > mlkit_gc_main_end) mlkit_gc_main_end = p->end;
  }
}

static void invalid(const char *reason) {
  fprintf(stderr, "GC image metadata: %s\n", reason);
  abort();
}
void mlkit_gc_register_static_image(const void *identity,
                                    const void *begin, const void *end,
                                    uintptr_t **roots, size_t root_count) {
  for (Image *p = images; p; p = p->next)
    if (p->identity == identity) invalid("image registered twice");
  for (Image *p = main_images; p; p = p->next)
    if (p->identity == identity) invalid("image registered twice");
  if ((uintptr_t)begin > (uintptr_t)end)
    invalid("invalid image bounds");
  Image *p = calloc(1, sizeof(*p));
  if (!p) invalid("out of memory");
  p->identity = identity;
  p->begin = (uintptr_t)begin;
  p->end = (uintptr_t)end;
  p->roots = roots;
  p->root_count = root_count;
  p->next = images;
  images = p;
  mlkit_gc_dynamic_images++;
}
void mlkit_gc_seal_main_image(void) {
  if (main_sealed) invalid("main image sealed twice");
  main_sealed = 1;
  main_images = images;
  images = NULL;
  mlkit_gc_dynamic_images = 0;
  main_bounds();
}
void mlkit_gc_unregister_static_image(const void *identity) {
  Image **link = &images;
  while (*link && (*link)->identity != identity) link = &(*link)->next;
  int is_main = !*link;
  if (is_main) {
    link = &main_images;
    while (*link && (*link)->identity != identity) link = &(*link)->next;
  }
  if (!*link) invalid("unknown image");
  Image *p = *link;
  *link = p->next;
  free(p);
  if (is_main) main_bounds();
  else mlkit_gc_dynamic_images--;
}
int mlkit_gc_static_pointer(const void *ptr) {
  uintptr_t v = (uintptr_t)ptr;
  if (v >= mlkit_gc_main_begin && v < mlkit_gc_main_end) return 1;
  for (Image *p = images; p; p = p->next)
    if (v >= p->begin && v < p->end) return 1;
  return 0;
}
void mlkit_gc_visit_static_roots(uintptr_t (*visit)(uintptr_t)) {
  for (unsigned list = 0; list < 2; list++)
    for (Image *p = list ? images : main_images; p; p = p->next)
      for (size_t i = 0; i < p->root_count; i++)
        *p->roots[i] = visit(*p->roots[i]);
}
