#include <assert.h>
#include <stddef.h>
#include <sys/types.h>
extern long disable_gc;
extern size_t time_to_gc;
extern ssize_t num_gc;
extern long arm64_deferred_hook(long);
extern long arm64_deferred_leaf(long);
static ssize_t before;

long arm64_deferred_inner(long x) {
  assert(disable_gc == 1 && time_to_gc == 1);
  long result = arm64_deferred_leaf(x);
  assert(disable_gc == 1 && time_to_gc == 1 && num_gc == before);
  return result;
}

long arm64_deferred_outer(long x) {
  assert(disable_gc == 1);
  before = num_gc;
  time_to_gc = 1;
  long result = arm64_deferred_hook(x);
  assert(disable_gc == 1 && time_to_gc == 1 && num_gc == before);
  return result;
}

long arm64_deferred_verify(void) {
  assert(num_gc > before);
  return 1;
}
