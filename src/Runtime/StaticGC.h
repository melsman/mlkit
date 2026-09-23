#ifndef MLKIT_STATIC_GC_H
#define MLKIT_STATIC_GC_H
#include <stddef.h>
#include <stdint.h>

/* Static bounds/root cells remain owned by the image until unregistration.
 * identity uniquely names the image; there is no return-PC index. */
void mlkit_gc_register_static_image(const void *, const void *, const void *,
                               uintptr_t **, size_t);
/* Seal only the initial executable's registrations. Later dlopen images
 * retain exact bounds; they must never widen the executable's envelope. */
void mlkit_gc_seal_main_image(void);
extern uintptr_t mlkit_gc_main_begin, mlkit_gc_main_end;
extern size_t mlkit_gc_dynamic_images;
void mlkit_gc_unregister_static_image(const void *);
int mlkit_gc_static_pointer(const void *);
static inline int mlkit_gc_in_static_data(const void *ptr) {
  uintptr_t v = (uintptr_t)ptr;
  return (v >= mlkit_gc_main_begin && v < mlkit_gc_main_end) ||
    (mlkit_gc_dynamic_images && mlkit_gc_static_pointer(ptr));
}
void mlkit_gc_visit_static_roots(uintptr_t (*)(uintptr_t));
#endif
