#ifndef MLKIT_ARM64_GC_H
#define MLKIT_ARM64_GC_H
#include <stddef.h>
#include <stdint.h>

typedef struct {
  uintptr_t pc;
  const uintptr_t *anchor;
} MLKitArm64Frame;

/* Metadata remains owned by the image until it is unregistered. */
void mlkit_arm64_register_image(const MLKitArm64Frame *, size_t,
                               const void *, const void *, uintptr_t **, size_t);
void mlkit_arm64_unregister_image(const MLKitArm64Frame *);
int mlkit_arm64_static_pointer(const void *);
void mlkit_arm64_visit_roots(uintptr_t *, uintptr_t, uintptr_t (*)(uintptr_t));
#endif
