/* Generated/C allocation interoperability and a repeatable timing workload. */
#include <assert.h>
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <time.h>
#include "Spawn.h"

extern uintptr_t *arm64_generated_alloc(Region, size_t);
#define THREADS 4
#ifndef ALLOCS
#define ALLOCS 16384
#endif
static int ready, start;
static size_t words = 32;
static size_t allocations = ALLOCS;
static int mixed = 1, private_regions = 0;
typedef struct {
  Region region;
  uintptr_t *cells[ALLOCS];
  size_t id;
} Worker;
static Worker workers[THREADS];
static void yield(void) {
#ifdef ARGOBOTS
  ABT_thread_yield();
#else
  sched_yield();
#endif
}
static void fill(Worker *w, size_t i) {
  uintptr_t *p = mixed && (i & 1) ? alloc(w->region, words)
                                 : arm64_generated_alloc(w->region, words);
  w->cells[i] = p;
  for (size_t j = 0; j < words; ++j) p[j] = (w->id * ALLOCS + i) * words + j;
}
static void *work(ThreadInfo *ti) {
  thread_init(ti);
  Worker *w = ti->arg;
  Ro ro;
  if (private_regions) w->region = allocateRegion(&ti->ctx, &ro, 0);
  __atomic_add_fetch(&ready, 1, __ATOMIC_RELEASE);
  while (!__atomic_load_n(&start, __ATOMIC_ACQUIRE)) yield();
  for (size_t i = 0; i < allocations; ++i) fill(w, i);
  /* Private pages must remain alive until verification is complete. */
  if (private_regions) {
    for (size_t i = 0; i < allocations; ++i)
      for (size_t j = 0; j < words; ++j)
        assert(w->cells[i][j] == (w->id * ALLOCS + i) * words + j);
    deallocateRegion(&ti->ctx);
  }
  thread_exit(NULL);
  return NULL;
}
static void boundaries(Context ctx, int protect) {
  Ro ro;
  Region r = allocateRegion(ctx, &ro, protect);
  uintptr_t *first = ro.g0.a;
  uintptr_t *p = arm64_generated_alloc(r, ALLOCATABLE_WORDS_IN_REGION_PAGE);
  assert(p == first);
  assert(ro.g0.a == first + ALLOCATABLE_WORDS_IN_REGION_PAGE);
  p[ALLOCATABLE_WORDS_IN_REGION_PAGE - 1] = 123;
  uintptr_t *next = arm64_generated_alloc(r, 1);
  assert(next != first + ALLOCATABLE_WORDS_IN_REGION_PAGE);
  assert(p[ALLOCATABLE_WORDS_IN_REGION_PAGE - 1] == 123);
  uintptr_t *large = arm64_generated_alloc(r, 2048);
  large[0] = 456;
  large[2047] = 789;
  assert(large[0] == 456 && large[2047] == 789);
  deallocateRegion(ctx);
}
void code(Context ctx) {
  boundaries(ctx, 0);
  boundaries(ctx, 1);
  const char *scenario = getenv("ALLOCATION_SCENARIO");
  int single = scenario && strcmp(scenario, "single") == 0;
  private_regions = scenario && strcmp(scenario, "private") == 0;
  mixed = !scenario || strcmp(scenario, "mixed") == 0;
  if (scenario && strcmp(scenario, "expansion") == 0) {
    words = 1000;
    allocations = ALLOCS / 32;
  }
  struct timespec begin, end;
  clock_gettime(CLOCK_MONOTONIC, &begin);
  Ro ro;
  Region r = allocateRegion(ctx, &ro, 1);
  for (size_t t = 0; t < THREADS; ++t) {
    workers[t].region = r;
    workers[t].id = t;
  }
  if (single) {
    for (size_t i = 0; i < allocations; ++i) fill(&workers[0], i);
  } else {
    ThreadInfo *threads[THREADS];
    for (size_t t = 0; t < THREADS; ++t) threads[t] = thread_create(work, &workers[t]);
    while (__atomic_load_n(&ready, __ATOMIC_ACQUIRE) != THREADS) yield();
    __atomic_store_n(&start, 1, __ATOMIC_RELEASE);
    for (size_t t = 0; t < THREADS; ++t) thread_get(threads[t]);
    for (size_t t = 0; t < THREADS; ++t) thread_free(threads[t]);
  }
  if (!private_regions)
    for (size_t t = 0; t < (single ? 1 : THREADS); ++t)
      for (size_t i = 0; i < allocations; ++i)
        for (size_t j = 0; j < words; ++j)
          assert(workers[t].cells[i][j] == (t * ALLOCS + i) * words + j);
  deallocateRegion(ctx);
  assert(ctx->topregion == NULL);
  clock_gettime(CLOCK_MONOTONIC, &end);
  if (getenv("ALLOCATION_TIMING"))
    fprintf(stderr, "%.9f\n", (double)(end.tv_sec - begin.tv_sec) +
            (end.tv_nsec - begin.tv_nsec) / 1e9);
  puts("generated/C allocation passed");
  thread_finalize();
  exit(0);
}
