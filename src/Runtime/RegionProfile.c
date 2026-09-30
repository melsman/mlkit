#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
/* Cooperative sampled region snapshots. Never scan object/page contents. */
#include "RegionProfile.h"
#include "String.h"
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <time.h>
#include <string.h>
#include <sys/time.h>
#include <errno.h>
#include <limits.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#ifdef __linux__
#include <sched.h>
#endif
#ifdef PARALLEL
#include "Spawn.h"
#endif
_Static_assert(ATOMIC_INT_LOCK_FREE == 2, "profiler requests need lock-free signal-safe atomics");

/* Generated ML code overrides these weak defaults. Volatile prevents GCC
 * from folding their initializers before the linker resolves the symbols. */
__attribute__((weak)) const volatile uintptr_t mlkit_rp_capable = 0;
__attribute__((weak)) const char * const *mlkit_rp_main_source_slot;
#ifdef ENABLE_GC
#define RP_GC_ENABLED "true"
#else
#define RP_GC_ENABLED "false"
#endif
static uint64_t gc_collections;
void mlkit_rp_gc_completed(void) { gc_collections++; }
int mlkit_rp_enabled;
/* Count assigned pages, including GC from/to-space overlap, but not cached
 * pages. No per-region counters; releases traverse links, never page contents.
 * These counters remain active while snapshot collection is paused. */
static _Atomic uint64_t assigned_pages, maximum_pages;
void mlkit_rp_page_alloc(void) {
  uint64_t n = atomic_fetch_add_explicit(&assigned_pages, 1, memory_order_relaxed)+1;
  uint64_t peak = atomic_load_explicit(&maximum_pages, memory_order_relaxed);
  while (peak < n && !atomic_compare_exchange_weak_explicit(
           &maximum_pages, &peak, n, memory_order_relaxed, memory_order_relaxed)) {}
}
void mlkit_rp_pages_free(Rp *p) {
  uint64_t n = 0;
  for (; p; p = p->n) n++;
  atomic_fetch_sub_explicit(&assigned_pages, n, memory_order_relaxed);
}

_Atomic int mlkit_rp_pending;
uint64_t mlkit_rp_interval_us = 10000;
int mlkit_rp_report;
int mlkit_rp_gc_samples;
int mlkit_rp_gc_major = -1;
static uint64_t capture_ns, total_pages, max_delay_ns, next_due_ns;
static uint64_t traversal_ns, serialization_ns, wait_ns, cpu_ns, total_frames, coalesced, skipped, peak_bytes;
static struct sigaction previous_alarm;
static int timer_installed;
int mlkit_rp_initially_paused;
const char *mlkit_rp_filename = "profile.rp";
const char *mlkit_rp_control;
static int control_fd = -1;
static FILE *output;
static int active;
static uint64_t sequence, request_time, sample_wait;
static struct timespec origin;

#ifdef PARALLEL
static pthread_mutex_t registry_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t large_lock = PTHREAD_MUTEX_INITIALIZER;
#define LOCK() pthread_mutex_lock(&registry_lock)
#define UNLOCK() pthread_mutex_unlock(&registry_lock)
#else
#define LOCK() ((void)0)
#define UNLOCK() ((void)0)
#endif
typedef struct Participant {
  Context ctx;
  uint64_t id;
  int worker, cpu, stable; /* 0: executing, 1: parked, 2: join, 3: not started */
  uintptr_t *base;
  const uintptr_t *map;
  struct Participant *next;
} Participant;
static Participant *participants;
static int rendezvous, serializing;
static uint64_t record_thread;
static int record_worker = -1, record_cpu = -1;

typedef struct Large {
  void *address;
  uint64_t bytes;
  struct Large *next;
} Large;
#define LARGE_BUCKETS 4093
static Large *large[LARGE_BUCKETS];
static size_t bucket(void *p) { return ((uintptr_t)p >> 3) % LARGE_BUCKETS; }
static void fail(const char *s) {
  fprintf(stderr, "region profiler: %s\n", s);
  _Exit(EXIT_FAILURE);
}
static void *checked_alloc(size_t n) {
  void *p = malloc(n);
  if (!p) fail("out of memory");
  return p;
}
void mlkit_rp_large_alloc(void *p, size_t words) {
  if (!mlkit_rp_enabled) return;
#ifdef PARALLEL
  pthread_mutex_lock(&large_lock);
#endif
  Large *entry = checked_alloc(sizeof(*entry));
  entry->address = p;
  entry->bytes = (uint64_t)words * sizeof(uintptr_t);
  size_t h = bucket(p);
  entry->next = large[h];
  large[h] = entry;
#ifdef PARALLEL
  pthread_mutex_unlock(&large_lock);
#endif
}
void mlkit_rp_large_free(void *p) {
  if (!mlkit_rp_enabled) return;
#ifdef PARALLEL
  pthread_mutex_lock(&large_lock);
#endif
  Large **link = &large[bucket(p)];
  while (*link && (*link)->address != p) link = &(*link)->next;
  if (!*link) fail("large object missing from size table");
  Large *entry = *link;
  *link = entry->next;
  free(entry);
#ifdef PARALLEL
  pthread_mutex_unlock(&large_lock);
#endif
}
static uint64_t large_size(void *p) {
  for (Large *e = large[bucket(p)]; e; e = e->next)
    if (e->address == p) return e->bytes;
  fail("large object missing from size table");
  return 0;
}
static uint64_t timestamp(void) {
  struct timespec now;
  if (clock_gettime(CLOCK_MONOTONIC, &now)) fail("cannot read clock");
  int64_t ns = (int64_t)(now.tv_sec-origin.tv_sec)*INT64_C(1000000000)
             + now.tv_nsec-origin.tv_nsec;
  return (uint64_t)ns;
}
static Participant *participant(Context ctx) {
  for (Participant *p = participants; p; p = p->next) if (p->ctx == ctx) return p;
  fail("unregistered ML thread");
  return NULL;
}
/* Never wait with a region lock held. Argobots must release its execution
 * stream so that runnable ULTs can publish their own stack anchors. */
static void progress(void) {
  UNLOCK();
#ifdef ARGOBOTS
  ABT_thread_yield();
#elif defined(PARALLEL)
  struct timespec delay = {0,100000};
  nanosleep(&delay, NULL);
#endif
  LOCK();
}
static void await_release(void) { while (rendezvous) progress(); }
static void await_output(void) { while (serializing) progress(); }
static void set_anchor(Participant *p, uintptr_t *base, const uintptr_t *map) {
  p->base = base; p->map = map;
#ifdef __linux__
  p->cpu = sched_getcpu();
#else
  p->cpu = -1; /* No portable current-CPU query on this platform. */
#endif
#ifdef ARGOBOTS
  p->worker = execution_stream_rank();
#else
  p->worker = -1;
#endif
}
void mlkit_rp_thread_create(Context ctx, int id) {
  if (!mlkit_rp_enabled) return;
  LOCK();
  Participant *p = checked_alloc(sizeof(*p));
  *p = (Participant){.ctx=ctx,.id=(uint64_t)id,.worker=-1,.cpu=-1,.stable=3,.next=participants};
  participants = p;
  await_output();
  if (output) fprintf(output, "{\"type\":\"thread_start\",\"thread\":%d,\"time\":%" PRIu64 "}\n", id, timestamp());
  UNLOCK();
}
void mlkit_rp_thread_enter(Context ctx) {
  if (!mlkit_rp_enabled) return;
  LOCK();
  Participant *p = participant(ctx);
  await_release(); p->stable = 0;
  UNLOCK();
}
void mlkit_rp_thread_exit(Context ctx) {
  if (!mlkit_rp_enabled) return;
  LOCK();
  Participant **link = &participants;
  while (*link && (*link)->ctx != ctx) link = &(*link)->next;
  if (!*link) fail("unregistered thread exit");
  Participant *p = *link;
  /* No ML frame remains after the closure returns. */
  *link = p->next;
  await_output();
  if (output) fprintf(output, "{\"type\":\"thread_end\",\"thread\":%" PRIu64 ",\"time\":%" PRIu64 "}\n", p->id, timestamp());
  free(p);
  UNLOCK();
}
uintptr_t mlkit_rp_wait_enter(Context ctx, uintptr_t *base, const uintptr_t *map) {
  if (!mlkit_rp_enabled) return 1;
  LOCK();
  Participant *p = participant(ctx);
  set_anchor(p,base,map); p->stable = 2;
  UNLOCK(); return 1;
}
uintptr_t mlkit_rp_wait_leave(Context ctx) {
  if (!mlkit_rp_enabled) return 1;
  LOCK();
  Participant *p = participant(ctx);
  await_release(); p->stable = 0;
  UNLOCK(); return 1;
}
static void quoted_bytes(const char *s, size_t n) {
  fputc('"', output);
  for (const unsigned char *p = (const unsigned char *)s; n; p++, n--) {
    if (*p == '"' || *p == '\\') fprintf(output, "\\%c", *p);
    else if (*p < 32 || *p >= 127) fprintf(output, "\\u%04x", *p);
    else fputc(*p, output);
  }
  fputc('"', output);
}
/* The handler only requests work. It never touches an ML stack or stdio. */
static void request_sample(int sig) { (void)sig; mlkit_rp_pending = 1; }
int mlkit_rp_parse_interval(const char *s) {
  if (!strcmp(s, "0")) { mlkit_rp_interval_us = 0; return 1; }
  if (*s < '0' || *s > '9') return 0;
  errno = 0;
  char *end;
  unsigned long long n = strtoull(s, &end, 10);
  uint64_t scale = !strcmp(end, "ms") ? 1000 : !strcmp(end, "s") ? 1000000 : 0;
  if (errno || !scale || n > (uint64_t)INT_MAX*1000000/scale) return 0;
  mlkit_rp_interval_us = n*scale;
  return 1;
}
static void timer_state(int running) {
  if (!timer_installed) return;
  struct itimerval timer = {0};
  if (running || control_fd >= 0) {
    uint64_t interval = mlkit_rp_interval_us ? mlkit_rp_interval_us : 10000;
    timer.it_value.tv_sec = interval/1000000;
    timer.it_value.tv_usec = interval%1000000;
    timer.it_interval = timer.it_value;
  }
  if (setitimer(ITIMER_REAL, &timer, NULL)) fail("cannot set sampling timer");
  mlkit_rp_pending = 0;
  next_due_ns = timestamp()+mlkit_rp_interval_us*1000;
}
void mlkit_rp_close(void) {
  LOCK();
  await_output();
  if (!output) { UNLOCK(); return; }
  if (control_fd >= 0) { close(control_fd); control_fd = -1; unlink(mlkit_rp_control); }
  timer_state(0);
  /* Keep the harmless handler until process exit: another OS thread may
   * still have an already-delivered SIGALRM queued after timer disarm. */
  timer_installed = 0;
  if (mlkit_rp_report)
    fprintf(stderr, "region profiler: samples=%" PRIu64 " pages_visited=%" PRIu64
            " frames=%" PRIu64 " coalesced=%" PRIu64 " skipped=%" PRIu64
            " capture_ns=%" PRIu64 " traversal_ns=%" PRIu64 " serialization_ns=%" PRIu64
            " wait_ns=%" PRIu64 " cpu_ns=%" PRIu64 " max_delay_ns=%" PRIu64 " sampled_peak_bytes=%" PRIu64 " max_pages=%" PRIu64 "\n",
            sequence, total_pages, total_frames, coalesced, skipped, capture_ns,
            traversal_ns, serialization_ns, wait_ns, cpu_ns, max_delay_ns, peak_bytes, atomic_load(&maximum_pages));
  for (Participant *p = participants; p; p = p->next)
    fprintf(output,"{\"type\":\"thread_end\",\"thread\":%" PRIu64
            ",\"time\":%" PRIu64 ",\"reason\":\"process_exit\"}\n",p->id,timestamp());
  fprintf(output,"{\"type\":\"session_end\",\"time\":%" PRIu64
          ",\"samples\":%" PRIu64 ",\"max_pages\":%" PRIu64 ",\"gc_collections\":%" PRIu64 "}\n",timestamp(),sequence,atomic_load(&maximum_pages),gc_collections);
  FILE *f = output;
  output = NULL;
  if (fclose(f)) fail("cannot close profile output");
  UNLOCK();
}
void mlkit_rp_init(void) {
  if (!mlkit_rp_enabled) return;
#if defined(PARALLEL) && defined(ENABLE_GC)
  fail("GC plus parallel profiling is not supported");
#endif
#if defined(PROFILING)
  fail("cannot combine sampled profiling with the old -prof runtime");
#endif
  if (mlkit_rp_capable != MLKIT_RP_MAGIC)
    fail("recompile the executable and its ML libraries with -region_profile");
#ifndef ENABLE_GC
  if (mlkit_rp_gc_samples) fail("-rp_gc_samples requires a GC runtime");
#endif
  output = fopen(mlkit_rp_filename, "w");
  if (!output) fail("cannot open profile output");
  if (clock_gettime(CLOCK_MONOTONIC, &origin)) fail("cannot read clock");
  active = !mlkit_rp_initially_paused;
  fprintf(output, "{\"type\":\"header\",\"format\":\"mlkit-region-profile\",\"version\":3,\"time_unit\":\"ns\",\"size_unit\":\"bytes\",\"word_bytes\":%zu,\"page_bytes\":%zu,\"gc_enabled\":%s,\"main_source\":",
          sizeof(uintptr_t), sizeof(Rp), RP_GC_ENABLED);
  const char *main_source = mlkit_rp_main_source_slot ? *mlkit_rp_main_source_slot : "unknown source";
  quoted_bytes(main_source,strlen(main_source));
  fputs("}\n",output);
  if (fflush(output)) fail("cannot write profile header");
  if (mlkit_rp_control) {
    struct sockaddr_un address = {0};
    address.sun_family = AF_UNIX;
    if (strlen(mlkit_rp_control) >= sizeof(address.sun_path)) fail("control socket path too long");
    strcpy(address.sun_path, mlkit_rp_control);
    control_fd = socket(AF_UNIX, SOCK_DGRAM, 0);
    if (control_fd < 0) fail("cannot create control socket");
    mode_t previous = umask(0077);
    int rc = bind(control_fd, (struct sockaddr *)&address, sizeof(address));
    umask(previous);
    /* Never unlink an existing path: it may belong to another process. */
    if (rc || fcntl(control_fd, F_SETFL, O_NONBLOCK) < 0 ||
        fcntl(control_fd, F_SETFD, FD_CLOEXEC) < 0) fail("cannot bind control socket");
  }
  if (mlkit_rp_interval_us || control_fd >= 0) {
    struct itimerval old;
    if (getitimer(ITIMER_REAL, &old) || sigaction(SIGALRM, NULL, &previous_alarm))
      fail("cannot inspect sampling timer");
    if (old.it_value.tv_sec || old.it_value.tv_usec || previous_alarm.sa_handler != SIG_DFL)
      fail("SIGALRM/ITIMER_REAL already in use; use -rp_interval 0 without -rp_control");
    struct sigaction action = {0};
    action.sa_handler = request_sample;
    action.sa_flags = SA_RESTART;
    sigemptyset(&action.sa_mask);
    if (sigaction(SIGALRM, &action, NULL)) fail("cannot install sampling timer");
    timer_installed = 1;
    timer_state(active);
  }
  if (atexit(mlkit_rp_close)) fail("cannot register output cleanup");
}

static Region *seen;
static size_t seen_count, seen_capacity;
static int remember(Region r) {
  for (size_t i = 0; i < seen_count; i++) if (seen[i] == r) return 0;
  if (seen_count == seen_capacity) {
    seen_capacity = seen_capacity ? 2*seen_capacity : 32;
    Region *p = realloc(seen, seen_capacity*sizeof(*p));
    if (!p) fail("out of memory");
    seen = p;
  }
  seen[seen_count++] = r;
  return 1;
}
typedef struct Record {
  const char *unit, *name, *source;
  uint64_t id, thread, pages, tail, big, finite, desc;
  int worker, infinite, cpu;
  uintptr_t run_type;
  uint64_t g0_pages, g0_tail, g1_pages, g1_tail;
} Record;
static Record *records;
static size_t record_count, record_capacity;
static void save_record(Record r) {
  if (record_count == record_capacity) {
    record_capacity = record_capacity ? record_capacity*2 : 128;
    Record *p = realloc(records,record_capacity*sizeof(*p));
    if (!p) fail("out of memory");
    records = p;
  }
  records[record_count++] = r;
}
static void region_record(const char *unit, const char *name, const char *source, uint64_t id, uintptr_t *storage,
                          uintptr_t words, uintptr_t run_type, uint64_t *pages_visited) {
  uint64_t pages = 0, tail = 0, big = 0, finite = 0, desc = 0;
  uint64_t g0_pages = 0, g0_tail = 0;
  if (words == UINTPTR_MAX) {
    Region r = (Region)storage;
    if (!remember(r)) return;
    for (Rp *p = clear_fp(r->g0.fp); p; p = clear_tospace_bit(p->n)) pages++;
    tail = (uint64_t)(rpBoundary(r->g0.a)-r->g0.a)*sizeof(uintptr_t);
    g0_pages = pages; g0_tail = tail;
#ifdef ENABLE_GEN_GC
    for (Rp *p = clear_fp(r->g1.fp); p; p = clear_tospace_bit(p->n)) pages++;
    tail += (uint64_t)(rpBoundary(r->g1.a)-r->g1.a)*sizeof(uintptr_t);
#endif
    for (Lobjs *p = clear_lobj_bit(r->lobjs); p; p = clear_lobj_bit(p->next)) big += large_size(p);
    desc = sizeof(Ro);
  } else finite = (uint64_t)words*sizeof(uintptr_t);
  *pages_visited += pages;
  save_record((Record){unit,name,source,id,record_thread,pages,tail,big,finite,desc,
                       record_worker,words == UINTPTR_MAX,record_cpu,run_type,g0_pages,g0_tail,pages-g0_pages,tail-g0_tail});
}
/* Binding definitions are emitted once on first observation. The native unit
 * strings live in resident code images, including retained REPL libraries. */
typedef struct Definition {
  const char *unit;
  uint64_t id;
  struct Definition *next;
} Definition;
static Definition *definitions;
static void define_record(const Record *r) {
  for (Definition *d = definitions; d; d = d->next)
    if (d->id == r->id && !strcmp(d->unit,r->unit)) return;
  Definition *d = checked_alloc(sizeof(*d));
  *d = (Definition){r->unit,r->id,definitions}; definitions = d;
  fputs("{\"type\":\"binding\",\"unit\":",output);
  quoted_bytes(r->unit,strlen(r->unit));
  fprintf(output,",\"binding\":%" PRIu64 ",\"name\":",r->id);
  quoted_bytes(r->name,strlen(r->name));
  fputs("}\n",output);
}
static uint64_t count_cache(Rp *p) {
  uint64_t count = 0;
  for (; p; p = p->n) count++;
  return count;
}
static uint64_t cache_pages(void) {
  uint64_t pages;
  LOCK_LOCK(FREELISTMUTEX);
  pages = count_cache(global_freelist);
  LOCK_UNLOCK(FREELISTMUTEX);
#ifdef ARGOBOTS
  for (int i = 0; i < posixThreads; i++) pages += count_cache(freelists[i]);
#elif defined(PARALLEL)
  for (Participant *p = participants; p; p = p->next) pages += count_cache(p->ctx->freelist);
#endif
  return pages;
}
/* Values match Effect.ord_runType; zero means unavailable. */
static const char *run_type_name(uintptr_t type) {
  static const char *names[] = {"unavailable","string","pair","array","ref","triple","top","bot"};
  return type < sizeof(names)/sizeof(*names) ? names[type] : "unavailable";
}
/* Linker metadata maps global region pointer slots to their inferred types. */
typedef struct { Region *slot; uintptr_t type; } GlobalType;
__attribute__((weak)) const volatile GlobalType mlkit_rp_globals[] = {{NULL,0}};
static uintptr_t global_type(Region r) {
  for (const volatile GlobalType *g = mlkit_rp_globals; g->slot; g++)
    if (clearStatusBits(*g->slot) == r) return g->type;
  return 0;
}
static void write_record(const Record *r) {
  const char *unit = r->unit, *name = r->name;
  uint64_t id = r->id, pages = r->pages, tail = r->tail, big = r->big;
  uint64_t finite = r->finite, desc = r->desc;
  fprintf(output, "{\"type\":\"region\",\"sample\":%" PRIu64 ",\"thread\":%" PRIu64
          ",\"worker\":%d,\"cpu\":%d,\"unit\":", sequence, r->thread, r->worker, r->cpu);
  quoted_bytes(unit, strlen(unit));
  fputs(",\"source\":",output);
  quoted_bytes(r->source,strlen(r->source));
  fprintf(output, ",\"g0_pages\":%" PRIu64 ",\"g0_unused_tail\":%" PRIu64
          ",\"g1_pages\":%" PRIu64 ",\"g1_unused_tail\":%" PRIu64,
          r->g0_pages,r->g0_tail,r->g1_pages,r->g1_tail);
  fprintf(output,",\"region_type\":\"%s\"",run_type_name(r->run_type));
  fputs(",\"name\":", output);
  quoted_bytes(name, strlen(name));
  fprintf(output, ",\"binding\":%" PRIu64 ",\"kind\":\"%s\",\"pages\":%" PRIu64
          ",\"unused_tail\":%" PRIu64 ",\"page_footprint\":%" PRIu64
          ",\"large_bytes\":%" PRIu64 ",\"finite_bytes\":%" PRIu64
          ",\"descriptor_bytes\":%" PRIu64 "}\n", id,
          r->infinite ? "infinite" : "finite", pages, tail,
          pages*sizeof(Rp)-tail, big, finite, desc);
}
/* Validate all continuation chains before recording any bytes. A callback is
 * deliberately not a quiescent foreign boundary; timer requests remain pending. */
static int complete_chain(uintptr_t *base, const uintptr_t *map) {
  for (size_t frames = 0; frames < 1000000; frames++) {
    if (map[-1] != MLKIT_RP_MAGIC) fail("missing or incompatible ML frame metadata");
    if (map[-2] == UINTPTR_MAX) return 1;
    if (map[-2] == UINTPTR_MAX-1) return 0;
    uintptr_t *ret = base+map[-2];
    map = (const uintptr_t *)*ret;
    if (map[-1] != MLKIT_RP_MAGIC) fail("missing caller metadata: rebuild all ML libraries with -region_profile");
    if (map[-2] == UINTPTR_MAX) return 1;
    if (map[-2] == UINTPTR_MAX-1) return 0;
    uintptr_t *parent = ret+map[-3];
    if (parent <= base) fail("non-increasing ML frame chain");
    base = parent;
  }
  fail("invalid frame chain"); return 0;
}
typedef struct GlobalRegion {
  Region region;
  uint64_t id;
  struct GlobalRegion *next;
} GlobalRegion;
static GlobalRegion *globals;
static uint64_t next_global_id;
static uint64_t global_id(Region r) {
  for (GlobalRegion *g = globals; g; g = g->next) if (g->region == r) return g->id;
  GlobalRegion *g = checked_alloc(sizeof(*g));
  *g = (GlobalRegion){r,next_global_id++,globals}; globals = g;
  return g->id;
}
typedef struct StackRecord {
  uint64_t thread, active, finite;
  int worker, cpu;
} StackRecord;
static StackRecord *stacks;
static size_t stack_count, stack_capacity;
static void save_stack(uint64_t active, uint64_t finite) {
  if (finite > active) fail("finite reservations exceed active ML stack span");
  if (stack_count == stack_capacity) {
    stack_capacity = stack_capacity ? 2*stack_capacity : 16;
    StackRecord *p = realloc(stacks,stack_capacity*sizeof(*p));
    if (!p) fail("out of memory");
    stacks = p;
  }
  stacks[stack_count++] = (StackRecord){record_thread,active,finite,record_worker,record_cpu};
}
static void walk(Context ctx, uintptr_t *base, const uintptr_t *map,
                 uint64_t *frames, uint64_t *pages) {
  uintptr_t low = (uintptr_t)base, high = low;
  uint64_t finite = 0;
  for (;;) {
    if (map[-1] != MLKIT_RP_MAGIC) fail("missing or incompatible ML frame metadata");
    if (map[-2] == UINTPTR_MAX) break;
    if (map[-2] == UINTPTR_MAX-1) fail("cannot sample across a C-to-ML callback boundary");
    if (++*frames > 1000000 || map[-4] > 1000000) fail("invalid frame metadata");
    const char *unit = ((String)((uintptr_t)(map-5)+map[-5]))->data;
    const char *source = ((String)((uintptr_t)(map-6)+map[-6]))->data;
    for (uintptr_t i = 0; i < map[-4]; i++) {
      const uintptr_t *entry = map-7-5*i;
      if (entry[-2] != UINTPTR_MAX) {
        uint64_t bytes = entry[-2]*sizeof(uintptr_t);
        uintptr_t end = (uintptr_t)(base+entry[-1])+bytes;
        if (end > high) high = end; /* Includes spilled result reservations. */
        finite += bytes;
      }
      region_record(unit, entry[-3] ? ((String)((uintptr_t)(entry-3)+entry[-3]))->data : "", source, entry[0], base+entry[-1], entry[-2], entry[-4], pages);
    }
    uintptr_t *ret = base+map[-2];
    if ((uintptr_t)(ret+1) > high) high = (uintptr_t)(ret+1);
    map = (const uintptr_t *)*ret;
    if (map[-1] != MLKIT_RP_MAGIC) fail("missing caller metadata: rebuild all ML libraries with -region_profile");
    if (map[-2] == UINTPTR_MAX) break;
    if (map[-2] == UINTPTR_MAX-1) fail("cannot sample across a C-to-ML callback boundary");
    uintptr_t *parent = ret+map[-3];
    if (parent <= base) fail("non-increasing ML frame chain");
    base = parent;
  }
  save_stack(high-low,finite);
  /* Global regions outlive all compilation-unit calls and have no ML frame.
   * Locals already recorded through maps are deduplicated here. */
  for (Region r = ctx->topregion; r; r = r->p) {
    size_t i;
    for (i = 0; i < seen_count && seen[i] != r; i++) {}
    if (i == seen_count)
      region_record("<global>", "", "global", global_id(r), (uintptr_t *)r, UINTPTR_MAX, global_type(r), pages);
  }
}
static uintptr_t capture(Context ctx, uintptr_t *base, const uintptr_t *map, uintptr_t op) {
  if (!mlkit_rp_enabled) return 1;
  if (op >= 4 && !active) return 1;
  if (op == 3 && (!active || !mlkit_rp_pending)) return 1;
  if (op == 0 && active) return 1;
  if (op == 1 && !active) return 1;
  uint64_t start = timestamp(), frames = 0, pages = 0;
  uint64_t delay = op == 3 && start > next_due_ns ? start-next_due_ns : 0;
  if (delay > max_delay_ns) max_delay_ns = delay;
  /* Coalesce requests, including ticks received while serializing this sample. */
  mlkit_rp_pending = 0;
  sequence++;
  seen_count = 0;
  record_count = 0;
  stack_count = 0;
  if (participants) {
    for (Participant *p = participants; p; p = p->next) {
      record_thread = p->id; record_worker = p->worker; record_cpu = p->cpu;
      if (p->map) walk(p->ctx,p->base,p->map,&frames,&pages);
    }
  } else walk(ctx,base,map,&frames,&pages);
  uint64_t footprint = 0;
  for (size_t i = 0; i < record_count; i++)
    footprint += records[i].pages*sizeof(Rp)-records[i].tail+records[i].big+records[i].finite;
  if (footprint > peak_bytes) peak_bytes = footprint;
  uint64_t cached_pages = cache_pages();
  uint64_t captured = timestamp();
  traversal_ns += captured-start;
  /* Only copied values and resident unit strings are accessed below. */
  rendezvous = 0;
  serializing = 1;
  UNLOCK();
  for (size_t i = 0; i < record_count; i++) define_record(&records[i]);
  fprintf(output, "{\"type\":\"sample_begin\",\"sample\":%" PRIu64 ",\"time\":%" PRIu64
          ",\"requested_time\":%" PRIu64 ",\"wait_ns\":%" PRIu64
          ",\"gc_kind\":\"%s\",\"reason\":\"%s\"}\n", sequence, start, request_time, sample_wait,
          mlkit_rp_gc_major < 0 ? "none" : mlkit_rp_gc_major ? "major" : "minor", op == 0 ? "start" : op == 1 ? "pause" : op == 3 ? "periodic" : op == 4 ? "before_gc" : op == 5 ? "after_gc" : "explicit");
  for (size_t i = 0; i < record_count; i++) write_record(&records[i]);
  for (size_t i = 0; i < stack_count; i++) {
    const StackRecord *r = &stacks[i];
    fprintf(output,"{\"type\":\"stack\",\"sample\":%" PRIu64
            ",\"thread\":%" PRIu64 ",\"worker\":%d,\"cpu\":%d,\"active_bytes\":%" PRIu64
            ",\"finite_bytes\":%" PRIu64 ",\"stack_bytes\":%" PRIu64 "}\n",
            sequence,r->thread,r->worker,r->cpu,r->active,r->finite,r->active-r->finite);
  }
  fprintf(output, "{\"type\":\"sample_end\",\"sample\":%" PRIu64
          ",\"time\":%" PRIu64 ",\"frames\":%" PRIu64 ",\"pages_visited\":%" PRIu64 ",\"cache_pages\":%" PRIu64 ",\"cache_bytes\":%" PRIu64 ",\"max_pages\":%" PRIu64 ",\"gc_collections\":%" PRIu64 "}\n",
          sequence, timestamp(), frames, pages, cached_pages, cached_pages*sizeof(Rp), atomic_load(&maximum_pages),gc_collections);
  if (ferror(output)) fail("cannot write profile output");
  if (fflush(output)) fail("cannot flush profile output");
  LOCK();
  serializing = 0;
  serialization_ns += timestamp()-captured;
  capture_ns += timestamp()-start;
  total_pages += pages;
  total_frames += frames;
  if (op == 3 && mlkit_rp_interval_us && timestamp() > next_due_ns)
    coalesced += (timestamp()-next_due_ns)/(mlkit_rp_interval_us*1000);
  if (op == 0) { active = 1; timer_state(1); }
  if (op == 1) { active = 0; timer_state(0); }
  if (op == 3) timer_state(active);
  return 1;
}
uintptr_t mlkit_rp_capture(Context ctx, uintptr_t *base, const uintptr_t *map, uintptr_t op) {
  if (!mlkit_rp_enabled) return 1;
  LOCK();
  if (!output) { mlkit_rp_pending = 0; UNLOCK(); return 1; }
  if (!rendezvous && ((op == 0 && active) || (op == 1 && !active) ||
      (op >= 4 && !active) || (op == 3 && (!active || !mlkit_rp_pending || !mlkit_rp_interval_us)))) {
    if (op == 3) mlkit_rp_pending = 0;
    UNLOCK(); return 1;
  }
  uint64_t requested = timestamp();
  Participant *self = participants ? participant(ctx) : NULL;
  if (self) { set_anchor(self,base,map); self->stable = 1; }
  for (;;) {
    await_output();
    if (!rendezvous) break;
    await_release();
    /* A periodic participant has already contributed to that epoch. */
    if (op == 3) { if (self) self->stable = 0; UNLOCK(); return 1; }
    /* Capture releases the rendezvous before serialization. Recheck both
     * phases before becoming the next coordinator. */
  }
  if (self) {
    rendezvous = 1;
    mlkit_rp_pending = 1;
    uint64_t deadline = timestamp()+UINT64_C(1000000000);
    for (;;) {
      Participant *p = participants;
      while (p && p->stable) p = p->next;
      if (!p) break;
      if (timestamp() >= deadline) {
        rendezvous = 0; self->stable = 0; timer_state(active);
        /* Unknown foreign calls are not quiescent: never inspect their stacks. */
        if (op != 3) fail("cannot complete snapshot: a thread did not reach a safe point");
        skipped++;
        fprintf(output,"{\"type\":\"sample_skipped\",\"time\":%" PRIu64
                ",\"reason\":\"safe_point_timeout\"}\n",timestamp());
        UNLOCK(); return 1;
      }
      progress();
    }
  }
  sample_wait = timestamp()-requested;
  wait_ns += sample_wait;
  request_time = op == 3 && next_due_ns < requested ? next_due_ns : requested;
  int complete = 1;
  if (participants) {
    for (Participant *p = participants; p; p = p->next)
      if (p->map && !complete_chain(p->base,p->map)) complete = 0;
  } else complete = complete_chain(base,map);
  if (!complete) {
    rendezvous = 0;
    if (self) self->stable = 0;
    if (op != 3) fail("cannot sample across a C-to-ML callback boundary");
    UNLOCK(); return 1;
  }
  struct timespec cpu_before, cpu_after;
  clock_gettime(CLOCK_THREAD_CPUTIME_ID,&cpu_before);
  uintptr_t result = capture(ctx,base,map,op);
  clock_gettime(CLOCK_THREAD_CPUTIME_ID,&cpu_after);
  cpu_ns += (uint64_t)((int64_t)(cpu_after.tv_sec-cpu_before.tv_sec)*INT64_C(1000000000)
                     +cpu_after.tv_nsec-cpu_before.tv_nsec);
  rendezvous = 0;
  if (self) self->stable = 0;
  UNLOCK();
  return result;
}
uintptr_t mlkit_rp_poll(Context ctx, uintptr_t *base, const uintptr_t *map) {
  if (control_fd >= 0) {
    char command[32];
    /* Bound the work per poll, so an attached client cannot starve ML. */
    for (int i = 0; i < 8; i++) {
      ssize_t n = recv(control_fd, command, sizeof(command)-1, 0);
      if (n < 0) {
        if (errno != EAGAIN && errno != EWOULDBLOCK && errno != EINTR)
          fail("cannot read control socket");
        break;
      }
      command[n] = 0;
      if (!strcmp(command,"start")) mlkit_rp_capture(ctx,base,map,0);
      else if (!strcmp(command,"pause")) mlkit_rp_capture(ctx,base,map,1);
      else if (!strcmp(command,"sample")) mlkit_rp_capture(ctx,base,map,2);
      else if (!strcmp(command,"flush")) mlkit_rp_flush();
    }
  }
  return mlkit_rp_capture(ctx, base, map, 3);
}
/* Non-instrumented compilation remains usable with profiling disabled. */
uintptr_t mlkit_rp_start(void) { if (mlkit_rp_enabled) fail("start called from code without profiling metadata"); return 1; }
uintptr_t mlkit_rp_pause(void) { if (mlkit_rp_enabled) fail("pause called from code without profiling metadata"); return 1; }
uintptr_t mlkit_rp_sample(void) { if (mlkit_rp_enabled) fail("sample called from code without profiling metadata"); return 1; }
uintptr_t mlkit_rp_flush(void) {
  LOCK();
  await_output();
  if (output && fflush(output)) fail("cannot flush profile output");
  UNLOCK();
  return 1;
}
uintptr_t mlkit_rp_mark(String label) {
  LOCK();
  await_output();
  if (!output) { UNLOCK(); return 1; }
  fprintf(output, "{\"type\":\"mark\",\"time\":%" PRIu64 ",\"label\":", timestamp());
  quoted_bytes(label->data, sizeStringDefine(label));
  fputs("}\n", output);
  if (ferror(output)) fail("cannot write profile output");
  UNLOCK();
  return 1;
}

/* Between REPL commands there are persistent regions but no active ML frame. */
void mlkit_rp_idle(Context ctx) {
  static const uintptr_t end[] = {UINTPTR_MAX,MLKIT_RP_MAGIC};
  uintptr_t base = 0;
  mlkit_rp_poll(ctx,&base,end+2);
}
