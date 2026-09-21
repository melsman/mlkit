#ifndef MLKIT_ARM64_GC_H
#define MLKIT_ARM64_GC_H
#include <stddef.h>
#include <stdint.h>

/* Static bounds/root cells remain owned by the image until unregistration.
 * identity uniquely names the image; there is no return-PC index. */
void mlkit_arm64_register_static_image(const void *, const void *, const void *,
                               uintptr_t **, size_t);
void mlkit_arm64_unregister_static_image(const void *);
int mlkit_arm64_static_pointer(const void *);
void mlkit_arm64_visit_roots(uintptr_t *, uintptr_t, uintptr_t (*)(uintptr_t));
#endif
