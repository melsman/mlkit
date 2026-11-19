/* Exercise the old-generation mutable-region scan with a raw record prefix. */
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "Region.h"
#include "GC.h"
#include "Tagging.h"
#include "Exception.h"

static size_t static_data;
size_t *data_begin_addr = &static_data;
size_t *data_end_addr = &static_data;
Exception *exn_INTERRUPT, *exn_OVERFLOW;
void raise_exn(Context ctx, uintptr_t exn) { (void)ctx; (void)exn; abort(); }

void code(Context ctx)
{
  Ro records, contents;
  Region r = allocateArrayRegion(ctx, &records, 0);
  Region c = allocateRegion(ctx, &contents, 0);
  uintptr_t *child = alloc(c, 2);
  child[0] = val_tag_record(1);
  child[1] = convertIntToML(17);
  uintptr_t *record = alloc(r, 4);
  record[0] = gen_record_tag(3, 2, 0, 6);
  record[1] = 0x1000; /* Invalid pointers: scanning the prefix must fail. */
  record[2] = 0x2000;
  record[3] = (uintptr_t)child;

  uintptr_t sentinel[] = {UINTPTR_MAX, 0, 0};
  uintptr_t incoming[] = {0, (uintptr_t)(sentinel + 3)};
  uintptr_t snapshot[44] = {0};
  snapshot[43] = (uintptr_t)incoming;
  snapshot[31] = (uintptr_t)record;
  major_p = 0;
  gc(ctx, (uintptr_t **)snapshot, 1);
  record = (uintptr_t *)snapshot[31];
  assert(record[1] == 0x1000 && record[2] == 0x2000);
  assert(((uintptr_t *)record[3])[1] == convertIntToML(17));

  child = alloc(c, 2);
  child[0] = val_tag_record(1);
  child[1] = convertIntToML(42);
  record[3] = (uintptr_t)child;
  major_p = 0;
  gc(ctx, (uintptr_t **)snapshot, 1);
  record = (uintptr_t *)snapshot[31];
  assert(record[1] == 0x1000 && record[2] == 0x2000);
  assert(record[3] != (uintptr_t)child);
  assert(((uintptr_t *)record[3])[1] == convertIntToML(42));
  puts("mutable record old-to-young GC scan passed");
  exit(0);
}
