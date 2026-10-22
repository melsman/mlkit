#include <stdint.h>
extern uintptr_t preservation_hook(uintptr_t);
long preservation_seed(void) { return 11; }
/* The manual ML ABI passes tagged integers through unchanged. The exported
 * hook's C ABI converts them, so convert explicitly at this boundary. */
uintptr_t preservation_callback(uintptr_t x) {
#ifdef TAGGED
    return (preservation_hook(x >> 1) << 1) | 1;
#else
    return preservation_hook(x);
#endif
}
