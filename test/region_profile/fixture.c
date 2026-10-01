#include <stdint.h>
#include <assert.h>
uintptr_t rp_number(void) { return 3000; }
uintptr_t rp_pair(uintptr_t *pair) {
  assert(pair[0] == 3000 && pair[1] == 3000);
  return pair[0];
}
