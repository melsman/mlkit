#include <assert.h>
#include <stdint.h>
#include "Spawn.h"
extern intptr_t arm64_parallel_hook(intptr_t);
intptr_t arm64_parallel_context(intptr_t old) {
  assert(thread_info()->ctx.exnptr != (void *)old);
  return 23;
}
intptr_t arm64_parallel_callback(void) {
  Context ctx = &thread_info()->ctx;
  void *handler = ctx->exnptr;
  intptr_t result = arm64_parallel_hook((intptr_t)handler);
  assert(ctx->exnptr == handler);
  return result;
}
