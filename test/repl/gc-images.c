/* Test-only hooks: request collection at the next ML safe point. */
#define _GNU_SOURCE
#include <assert.h>
#include <dlfcn.h>
#include <stddef.h>
#include <sys/types.h>
extern size_t time_to_gc;
extern ssize_t num_gc;
extern long disable_gc;
static ssize_t before;
long repl_gc_request(void) {
  before = num_gc;
  time_to_gc = 1;
  return 1;
}
long repl_gc_check(void) {
  assert(num_gc > before);
  return 1;
}
long repl_gc_callback(void) {
  long (*hook)(long) = (long (*)(long))dlsym(RTLD_DEFAULT, "repl_gc_hook");
  assert(hook);
  long previous = disable_gc;
  before = num_gc;
  time_to_gc = 1;
  assert(hook(1) == 6001); /* ML tagged integers: 0 -> 3000. */
  assert(disable_gc == previous && num_gc == before && time_to_gc);
  return 1;
}
long repl_gc_in_callback(void) {
  static int nested;
  assert(disable_gc && num_gc == before);
  if (!nested) {
    long (*hook)(long) = (long (*)(long))dlsym(RTLD_DEFAULT, "repl_gc_hook");
    assert(hook);
    nested = 1;
    assert(hook(1) == 6001);
    nested = 0;
    assert(disable_gc && num_gc == before);
  }
  return 1;
}
