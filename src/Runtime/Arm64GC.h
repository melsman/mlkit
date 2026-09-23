#ifndef MLKIT_ARM64_GC_H
#define MLKIT_ARM64_GC_H
#include "StaticGC.h"
#include <stddef.h>
#include <stdint.h>

/* Static bounds/root cells remain owned by the image until unregistration.
 * identity uniquely names the image; there is no return-PC index. */
void mlkit_arm64_register_static_image(const void *, const void *, const void *,
                               uintptr_t **, size_t);
/* Seal only the initial executable's registrations. Later dlopen images
 * retain exact bounds; they must never widen the executable's envelope. */
void mlkit_arm64_seal_main_image(void);
#define mlkit_arm64_main_begin mlkit_gc_main_begin
#define mlkit_arm64_main_end mlkit_gc_main_end
#define mlkit_arm64_dynamic_images mlkit_gc_dynamic_images
void mlkit_arm64_unregister_static_image(const void *);
int mlkit_arm64_static_pointer(const void *);
static inline int mlkit_arm64_in_static_data(const void *ptr) {
  return mlkit_gc_in_static_data(ptr);
}
void mlkit_arm64_visit_roots(uintptr_t *, uintptr_t, uintptr_t (*)(uintptr_t));
#endif
