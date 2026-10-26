/* Instrument Region.c's context lookup, without changing the thread runtime. */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "Spawn.h"

static _Thread_local size_t lookups;

ThreadInfo *context_page_thread_info(void)
{
  ++lookups;
  return thread_info();
}

static void check_regions(Context ctx)
{
  Region previous = ctx->topregion;
  size_t before = lookups;
  for (size_t i = 0; i < 2000; ++i) {
    Ro ro;
    Region r = allocateRegion(ctx, &ro, 0);
    uintptr_t *p = alloc(r, 4);
    p[0] = i;
    p[3] = ~i;
    assert(p[0] == i && p[3] == ~i);
    deallocateRegion(ctx);
  }
  assert(lookups == before);
  assert(ctx->topregion == previous);

  /* A caller without a context still uses the public lookup-based wrapper. */
  Ro ro;
  Region r = allocateRegion(ctx, &ro, 0);
  Region raw = clearStatusBits(r);
  raw->g0.a = alloc_new_page(&raw->g0);
  assert(lookups > before);
  uintptr_t *p = alloc(r, 4);
  p[0] = 42;
  assert(p[0] == 42);
  deallocateRegion(ctx);
  assert(ctx->topregion == previous);
}

static void *worker(ThreadInfo *ti)
{
  thread_init(ti);
  check_regions(&ti->ctx);
  thread_exit(NULL);
  return NULL;
}

void code(Context ctx)
{
  check_regions(ctx);
  ThreadInfo *threads[4];
  for (size_t i = 0; i < 4; ++i) threads[i] = thread_create(worker, NULL);
  for (size_t i = 0; i < 4; ++i) {
    thread_get(threads[i]);
    thread_free(threads[i]);
  }
  puts("context-passing page allocation passed: main and four worker threads");
  thread_finalize();
  exit(0);
}
