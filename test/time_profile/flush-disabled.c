#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

extern uintptr_t mlkit_tp_flush(void);
static int reject_flush;

/* Interpose stdio to detect even an otherwise successful shared-stream flush. */
int fflush(FILE *stream) {
  (void)stream;
  if (reject_flush) {
    fputs("TimeProfile.flush touched stdio without a time session\n",stderr);
    _Exit(EXIT_FAILURE);
  }
  return 0;
}

uintptr_t tp_flush_disabled_check(void) {
  reject_flush = 1;
  uintptr_t result = mlkit_tp_flush();
  reject_flush = 0;
  return result;
}
