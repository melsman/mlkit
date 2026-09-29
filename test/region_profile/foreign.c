#include <stdatomic.h>
#include <stdint.h>
#include <errno.h>
#include <time.h>
static _Atomic int ready;
uintptr_t rp_foreign_ready(void) { return atomic_load(&ready); }
uintptr_t rp_foreign_block(void) {
  struct timespec remaining = {1,500000000};
  atomic_store(&ready,1);
  while (nanosleep(&remaining,&remaining) && errno == EINTR) {}
  return 1;
}
