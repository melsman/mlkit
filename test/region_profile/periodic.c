#include <stdint.h>
#include <stdlib.h>
uintptr_t rp_iterations(void) {
  const char *n = getenv("RP_ITERATIONS");
  return n ? strtoull(n,NULL,10) : 200000000;
}
