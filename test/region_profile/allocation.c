#include <stdint.h>
#include <assert.h>
#include <stdlib.h>
uintptr_t ap_pair(uintptr_t *p) { assert(p[0] == p[1]); return p[0]; }

uintptr_t ap_iterations(void) {
  const char *n = getenv("AP_ITERATIONS");
  return n ? strtoull(n,NULL,10) : 1000;
}
