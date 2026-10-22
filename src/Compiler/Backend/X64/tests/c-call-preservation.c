#include <stdint.h>
extern uintptr_t preservation_hook(uintptr_t);
long preservation_seed(void) { return 11; }
/* Exported ML hooks use ML integer representation, like this manual call. */
uintptr_t preservation_callback(uintptr_t x) { return preservation_hook(x); }
