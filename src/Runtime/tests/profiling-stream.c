/* Completed profiling samples must not corrupt subsequent malloc allocations. */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Profiling.h"
#include "Exception.h"

Exception *exn_INTERRUPT, *exn_OVERFLOW;
void raise_exn(Context ctx, uintptr_t exn)
{
  (void)ctx; (void)exn;
  abort();
}

void code(Context ctx)
{
  long marker;
  extern long *stackBot;
  profiling_off();
  profType = noTimer;
  profNo = 1;
  stackBot = &marker;
  for (unsigned sample = 0; sample < 100; ++sample) {
    profileTick(ctx, &marker);
    unsigned char *block = malloc(sizeof(TickList));
    assert(block);
    memset(block, 0xa5, sizeof(TickList));
    profileTick(ctx, &marker);
    for (size_t i = 0; i < sizeof(TickList); ++i) assert(block[i] == 0xa5);
    free(block);
  }
  outputProfilePost();
  puts("streamed profiling samples preserve unrelated allocations");
  exit(0);
}
