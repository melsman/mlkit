#define _POSIX_C_SOURCE 200809L
#include <assert.h>
#include <errno.h>
#include <stdint.h>
#include <stdatomic.h>
#include <time.h>
#include "Exception.h"
extern Context top_ctx;
extern _Atomic uintptr_t mlkit_tp_context;
#ifdef TP_GC
extern long disable_gc;
#define CHECK_GC() assert(disable_gc == 1)
#else
#define CHECK_GC() ((void)0)
#endif
extern long tp_hook(long), tp_leaf(long);
static void wait_ms(int ms) {
  struct timespec left = {ms/1000,(ms%1000)*1000000};
  while (nanosleep(&left,&left) && errno == EINTR) {}
}
long tp_inner(long x) {
  CHECK_GC();
  uintptr_t saved = atomic_load(&mlkit_tp_context);
  assert((saved & 3) == 2 && (saved & ~(uintptr_t)3));
  wait_ms(90);
  long result = tp_leaf(x);
  assert(atomic_load(&mlkit_tp_context) == saved);
  CHECK_GC();
  wait_ms(90);
  return result;
}
long tp_outer(long x) {
  CHECK_GC();
  uintptr_t saved = atomic_load(&mlkit_tp_context);
  assert((saved & 3) == 2 && (saved & ~(uintptr_t)3));
  wait_ms(90);
  long result = tp_hook(x);
  assert(atomic_load(&mlkit_tp_context) == saved);
  CHECK_GC();
  wait_ms(90);
  return result;
}

uintptr_t tp_throw(uintptr_t exn) {
  CHECK_GC();
  assert((atomic_load(&mlkit_tp_context) & 3) == 2);
  wait_ms(90);
  raise_exn(top_ctx, exn);
  return 1; /* nonlocal transfer */
}
