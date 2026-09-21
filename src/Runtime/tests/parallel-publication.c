/* Concurrent joins must publish the entire result, exactly once. Run with
 * pthreads and with both one and multiple Argobots execution streams. */
#include <assert.h>
#include <sched.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "Spawn.h"

#define JOINERS 8
#define WORDS 8192
#define ROUNDS 32
static uintptr_t payload[WORDS];
static int ready, start;
static ThreadInfo *target;
static int ids[JOINERS];
static void yield(void) {
#ifdef ARGOBOTS
  ABT_thread_yield();
#else
  sched_yield();
#endif
}
static void *produce(ThreadInfo *ti) {
  thread_init(ti);
  while (!__atomic_load_n(&start, __ATOMIC_ACQUIRE)) yield();
  for (size_t i = 0; i < WORDS; ++i) payload[i] = i * 17 + 3;
  thread_exit(payload);
  return NULL;
}
static void *child(ThreadInfo *ti) {
  thread_init(ti);
  /* Allocate and return protected region pages/mutexes to the freelists. */
  Ro ro;
  Region r = allocateRegion(&ti->ctx, &ro, 1);
  for (int i = 0; i < 1024; ++i) alloc(r, 32);
  deallocateRegion(&ti->ctx);
  thread_exit((void *)(uintptr_t)ti->tid);
  return NULL;
}
static void *consume(ThreadInfo *ti) {
  thread_init(ti);
  size_t id = (uintptr_t)ti->arg;
  __atomic_add_fetch(&ready, 1, __ATOMIC_RELEASE);
  while (!__atomic_load_n(&start, __ATOMIC_ACQUIRE)) yield();
  for (int n = 0; n < 64; ++n) {
    uintptr_t *p = thread_get(target);
    assert(p == payload);
    for (size_t i = 0; i < WORDS; ++i) assert(p[i] == i * 17 + 3);
  }
  ThreadInfo *t = thread_create(child, NULL);
  ids[id] = (int)(uintptr_t)thread_get(t);
  assert(ids[id] == t->tid);
  thread_free(t);
  thread_exit(NULL);
  return NULL;
}
void code(Context ctx) {
  (void)ctx;
  for (int round = 0; round < ROUNDS; ++round) {
    ready = start = 0;
    for (size_t i = 0; i < WORDS; ++i) payload[i] = 0;
    target = thread_create(produce, NULL);
    ThreadInfo *joiners[JOINERS];
    for (size_t j = 0; j < JOINERS; ++j)
      joiners[j] = thread_create(consume, (void *)(uintptr_t)j);
    while (__atomic_load_n(&ready, __ATOMIC_ACQUIRE) != JOINERS) yield();
    __atomic_store_n(&start, 1, __ATOMIC_RELEASE);
    for (size_t j = 0; j < JOINERS; ++j) thread_get(joiners[j]);
    assert(thread_get(target) == payload);
    for (size_t j = 0; j < JOINERS; ++j) {
      for (size_t k = j + 1; k < JOINERS; ++k) assert(ids[j] != ids[k]);
      thread_free(joiners[j]);
    }
    thread_free(target);
  }
  puts("parallel result publication passed");
  thread_finalize();
  exit(0);
}
