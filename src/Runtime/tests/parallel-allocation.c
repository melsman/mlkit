/* Exercise page publication and CAS allocation through the C runtime. */
#include <assert.h>
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include "Spawn.h"

static void yield(void) {
#ifdef ARGOBOTS
  ABT_thread_yield();
#else
  sched_yield();
#endif
}

#define THREADS 4
#define ALLOCS 8192
static int ready;
static int start;
typedef struct {
  Region region;
  size_t id;
  uintptr_t *cells[ALLOCS];
} Worker;
static Worker workers[THREADS];

static void *allocate(ThreadInfo *ti)
{
  thread_init(ti);
  Worker *w = ti->arg;
  __atomic_add_fetch(&ready, 1, __ATOMIC_RELEASE);
  while (!__atomic_load_n(&start, __ATOMIC_ACQUIRE)) yield();
  for (size_t i = 0; i < ALLOCS; ++i) {
    uintptr_t *p = alloc(w->region, 32);
    w->cells[i] = p;
    for (size_t j = 0; j < 32; ++j) p[j] = (w->id * ALLOCS + i) * 32 + j;
  }
  thread_exit(NULL);
  return NULL;
}

void code(Context ctx)
{
  Ro ro;
  Region r = allocateRegion(ctx, &ro, 1);
  ThreadInfo *threads[THREADS];
  for (size_t t = 0; t < THREADS; ++t) {
    workers[t].region = r;
    workers[t].id = t;
    threads[t] = thread_create(allocate, &workers[t]);
  }
  while (__atomic_load_n(&ready, __ATOMIC_ACQUIRE) != THREADS) yield();
  __atomic_store_n(&start, 1, __ATOMIC_RELEASE);
  for (size_t t = 0; t < THREADS; ++t) thread_get(threads[t]);
  for (size_t t = 0; t < THREADS; ++t)
    for (size_t i = 0; i < ALLOCS; ++i)
      for (size_t j = 0; j < 32; ++j)
        assert(workers[t].cells[i][j] == (t * ALLOCS + i) * 32 + j);
  deallocateRegion(ctx);
  assert(ctx->topregion == NULL);
  for (size_t t = 0; t < THREADS; ++t) thread_free(threads[t]);
  puts("parallel C allocation smoke test passed");
  thread_finalize();
  exit(0);
}
