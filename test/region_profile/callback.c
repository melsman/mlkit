#include <stdint.h>
extern uintptr_t rp_test_callback(uintptr_t);
uintptr_t rp_call_callback(void) { return rp_test_callback(7); }
