#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>
#include "Tagging.h"

uintptr_t recordCheck(double number, const char *text, intptr_t integer)
{
  if (number != 3.5 || strcmp(text, "after") || integer != 7) {
    fprintf(stderr, "mixed record read/store failed\n");
    exit(1);
  }
  puts("mixed record read/store passed");
  return 1;
}

uintptr_t recordLayoutCheck(uintptr_t *record)
{
#ifdef TAG_VALUES
  assert(get_record_size(record[0]) == 3);
  assert(get_record_skip(record[0]) == 1);
#else
  (void)record;
#endif
  return 1;
}
