#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
/* Cooperative region snapshots with selected-region object occupancy. */
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
#define RP_GC_ENABLED 1
#else
#define RP_GC_ENABLED 0
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
static int timer_installed;
int mlkit_rp_initially_paused;
const char *mlkit_rp_filename = "profile.rp";
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
static int current_cpu(void) {
#ifdef __linux__
  return sched_getcpu();
#else
  return -1; /* No portable current-CPU query on this platform. */
#endif
}
static void set_anchor(Participant *p, uintptr_t *base, const uintptr_t *map) {
  p->base = base; p->map = map;
  p->cpu = current_cpu();
#ifdef ARGOBOTS
  p->worker = execution_stream_rank();
#else
  p->worker = -1;
#endif
}
/* Portable wire format: magic/version, then LE uint32 payload length, byte tag,
 * fixed-order LE uint64 counters, and LE uint32-length-prefixed byte strings.
 * Never serialize native structs. One buffered stdio write per record. */
static unsigned char *wire;
static size_t wire_capacity;
static void little_endian(unsigned char *p, uint64_t n, size_t bytes) {
  for (size_t i = 0; i < bytes; i++, n >>= 8) p[i] = (unsigned char)n;
}
static void emit_record(unsigned char tag, const uint64_t *values, size_t count,
                        const char *const *strings, size_t nstrings, const size_t *lengths) {
  size_t size = 1 + count*8;
  for (size_t i = 0; i < nstrings; i++) {
    size_t n = lengths ? lengths[i] : strlen(strings[i]);
    if (n > UINT32_MAX-4 || size > UINT32_MAX-4-n) fail("profile record too large");
    size += 4+n;
  }
  if (size > SIZE_MAX-4) fail("profile record too large");
  if (size+4 > wire_capacity) {
    unsigned char *p = realloc(wire,size+4);
    if (!p) fail("out of memory");
    wire = p; wire_capacity = size+4;
  }
  little_endian(wire,size,4); wire[4] = tag;
  size_t offset = 5;
  for (size_t i = 0; i < count; i++, offset += 8) little_endian(wire+offset,values[i],8);
  for (size_t i = 0; i < nstrings; i++) {
    size_t n = lengths ? lengths[i] : strlen(strings[i]);
    little_endian(wire+offset,n,4); offset += 4;
    memcpy(wire+offset,strings[i],n); offset += n;
  }
  if (fwrite(wire,1,size+4,output) != size+4) fail("cannot write profile record");
}
#define NUMS(...) (const uint64_t[]){__VA_ARGS__}, sizeof((const uint64_t[]){__VA_ARGS__})/sizeof(uint64_t)
#define STRS(...) (const char *const[]){__VA_ARGS__}, sizeof((const char *const[]){__VA_ARGS__})/sizeof(char *), NULL
#define NO_STRINGS NULL, 0, NULL

/* Occupancy is counted only during snapshots. C allocations carry their
 * site tokens explicitly through REG_POLY_FUN_HDR / REG_POLY_CALL. */
__attribute__((weak)) const volatile uintptr_t mlkit_rp_allocation_capable = 0;
__attribute__((weak)) const char *mlkit_rp_build_id = "unknown";
uintptr_t mlkit_rp_allocation_enabled;
const char *mlkit_rp_region;
const char *mlkit_rp_expected_build;
static int all_regions(void) { return mlkit_rp_region && !strcmp(mlkit_rp_region,"all"); }
static const MlkitAllocationRegion *selected_region;
static const char *selected_unit, *selected_name, *selected_source;
static uint64_t selected_binding_id;
uintptr_t mlkit_rp_bind_region(Region r, const MlkitAllocationRegion *metadata) {
  r = clearStatusBits(r);
  if (!mlkit_rp_allocation_enabled || !mlkit_rp_region) return 1;
  if (all_regions()) { r->allocation_profile = metadata; return 1; }
  const char *colon = strrchr(mlkit_rp_region, ':');
  size_t n = (size_t)(colon-mlkit_rp_region);
  if (strlen(metadata->unit->data) == n &&
      !memcmp(metadata->unit->data,mlkit_rp_region,n) &&
      metadata->binding == strtoull(colon+1,NULL,10)) {
    r->allocation_profile = metadata;
    LOCK(); selected_region = metadata; UNLOCK();
  }
  return 1;
}
/* Indexed by Effect.ord_runType, but identities are the compiler's region
 * keys (Effect's toplevel region initialization), as printed by -Pcee. */
static const MlkitAllocationRegion *global_metadata(uintptr_t type) {
  static struct { size_t tag; char data[9]; } unit = {0,"<global>"};
  static struct { size_t tag; char data[7]; } source = {0,"global"};
  static const MlkitAllocationRegion metadata[] = {
#define GLOBAL_METADATA(n) {(String)&unit,(String)&source,(String)&source,n}
    GLOBAL_METADATA(0),GLOBAL_METADATA(3),GLOBAL_METADATA(4),GLOBAL_METADATA(5),
    GLOBAL_METADATA(6),GLOBAL_METADATA(7),GLOBAL_METADATA(1),GLOBAL_METADATA(2)
#undef GLOBAL_METADATA
  };
  if (type >= sizeof(metadata)/sizeof(*metadata)) fail("invalid global region type");
  return &metadata[type];
}
uintptr_t mlkit_rp_bind_global(Region r, uintptr_t type) {
  return mlkit_rp_bind_region(r,global_metadata(type));
}
typedef struct AllocationDefinition {
  const MlkitAllocationSite *site;
  uint64_t id;
  struct AllocationDefinition *next;
} AllocationDefinition;
static AllocationDefinition *allocation_definitions;
static uint64_t allocation_definition_count;
__attribute__((weak)) const char *const volatile mlkit_rp_ir_objects[][2] = {{NULL,NULL}};
static const char *allocation_object(const MlkitAllocationSite *site) {
  if (site) for (size_t i = 0; mlkit_rp_ir_objects[i][0]; i++)
    if (!strcmp(mlkit_rp_ir_objects[i][0],site->ir_identity->data))
      return mlkit_rp_ir_objects[i][1];
  return "";
}
static __attribute__((unused)) uint64_t allocation_definition(const MlkitAllocationSite *site) {
  for (AllocationDefinition *d = allocation_definitions; d; d = d->next)
    if (d->site == site) return d->id;
  AllocationDefinition *d = checked_alloc(sizeof(*d));
  *d = (AllocationDefinition){site,++allocation_definition_count,allocation_definitions};
  allocation_definitions = d;
  emit_record(13,NUMS(d->id,site ? site->id : 0,site ? site->kind : 2),
              STRS(site ? site->unit->data : "<runtime>",
                   site ? site->function->data : "runtime/unknown",
                   site ? site->source->data : "",
                   site ? site->ir_identity->data : "",allocation_object(site)));
  return d->id;
}
void mlkit_rp_thread_create(Context ctx, int id) {
  if (!mlkit_rp_enabled) return;
  LOCK();
  Participant *p = checked_alloc(sizeof(*p));
  *p = (Participant){.ctx=ctx,.id=(uint64_t)id,.worker=-1,.cpu=-1,.stable=3,.next=participants};
  participants = p;
  await_output();
  if (output) emit_record(2,NUMS(id,timestamp()),NO_STRINGS);
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
  if (output) emit_record(3,NUMS(p->id,timestamp()),STRS(""));
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
  if (running) {
    uint64_t interval = mlkit_rp_interval_us;
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
    emit_record(3,NUMS(p->id,timestamp()),STRS("process_exit"));
  if (mlkit_rp_allocation_enabled && !all_regions()) {
    if (selected_unit)
      emit_record(16,NUMS(selected_binding_id),STRS(selected_unit,selected_name,selected_source));
    else if (selected_region)
      emit_record(16,NUMS(selected_region->binding),STRS(selected_region->unit->data,
                  selected_region->name->data,selected_region->source->data));

  }
  emit_record(4,NUMS(timestamp(),sequence,atomic_load(&maximum_pages),gc_collections),NO_STRINGS);
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

  if (mlkit_rp_capable != MLKIT_RP_MAGIC)
    fail("recompile the executable and its ML libraries with -region_profile");
#ifndef ENABLE_GC
  if (mlkit_rp_gc_samples) fail("-rp_gc_samples requires a GC runtime");
#endif
  if (mlkit_rp_allocation_capable && mlkit_rp_allocation_capable != 4)
    fail("rebuild allocation profiling objects for the current runtime");
  if (mlkit_rp_expected_build && strcmp(mlkit_rp_expected_build,mlkit_rp_build_id))
    fail("allocation profile build identifier does not match this executable");
  if (mlkit_rp_region) {
    if (!all_regions()) {
      const char *colon = strrchr(mlkit_rp_region, ':');
      char *end;
      errno = 0;
      if (!colon || colon == mlkit_rp_region || colon[1] < '0' || colon[1] > '9')
        fail("-rp_region requires all or UNIT:BINDING from the viewer");
      (void)strtoull(colon+1,&end,10);
      if (errno || *end) fail("invalid region binding number");
    }
    if (mlkit_rp_allocation_capable != 4)
      fail("recompile all ML code with -rp");
    mlkit_rp_allocation_enabled = 1;
  }
  mlkit_rp_allocation_enabled = mlkit_rp_allocation_capable != 0;
  output = fopen(mlkit_rp_filename, "wb");
  if (!output) fail("cannot open profile output");
  if (clock_gettime(CLOCK_MONOTONIC, &origin)) fail("cannot read clock");
  active = !mlkit_rp_initially_paused;
  const unsigned char magic[] = {'M','L','K','R','P',0,10,0};
  if (fwrite(magic,1,sizeof(magic),output) != sizeof(magic)) fail("cannot write profile header");
  const char *main_source = mlkit_rp_main_source_slot ? *mlkit_rp_main_source_slot : "unknown source";
  emit_record(1,NUMS(sizeof(uintptr_t),sizeof(Rp),RP_GC_ENABLED),STRS(main_source));
  if (mlkit_rp_allocation_capable) emit_record(12,NUMS(mlkit_rp_allocation_enabled,1),STRS(mlkit_rp_build_id,mlkit_rp_region ? mlkit_rp_region : ""));
  if (mlkit_rp_allocation_enabled)
    for (size_t i = 0; mlkit_rp_ir_objects[i][0]; i++)
      emit_record(17,NUMS(),STRS(mlkit_rp_ir_objects[i][0],mlkit_rp_ir_objects[i][1]));
  if (fflush(output)) fail("cannot write profile header");
  if (mlkit_rp_interval_us) {
    struct sigaction previous_alarm;
    struct itimerval old;
    if (getitimer(ITIMER_REAL, &old) || sigaction(SIGALRM, NULL, &previous_alarm))
      fail("cannot inspect sampling timer");
    if (old.it_value.tv_sec || old.it_value.tv_usec || previous_alarm.sa_handler != SIG_DFL)
      fail("SIGALRM/ITIMER_REAL already in use; use -rp_interval 0");
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
  uint64_t definition;
  uint64_t payload, objects, object_overhead, slack, detailed;
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
#ifdef PROFILING

/* Snapshot-owned values. Addresses are decoded only while participants are
 * parked; no heap pointers survive into serialization. */
typedef struct Occupancy {
  size_t instance;
  const MlkitAllocationSite *site;
  uint64_t count, bytes;
  struct Occupancy *next, *hash_next;
} Occupancy;
static Occupancy *occupancy, *occupancy_hash[257];
static uint64_t occupied_payload, occupied_objects, occupied_slack;
static void count_object(size_t instance, const ObjectDesc *obj) {
  uintptr_t token = objectDescPoint(obj);
  const MlkitAllocationSite *site = token <= 1 ? NULL : (const MlkitAllocationSite *)(token << 3);
  size_t h = token % 257;
  Occupancy *o;
  for (o = occupancy_hash[h]; o && o->site != site; o = o->hash_next) {}
  if (!o) {
    o = checked_alloc(sizeof(*o));
    *o = (Occupancy){instance,site,0,0,occupancy,occupancy_hash[h]};
    occupancy = o; occupancy_hash[h] = o;
  }
  uint64_t bytes = objectDescSize(obj)*sizeof(uintptr_t);
  o->count++; o->bytes += bytes;
  occupied_objects++; occupied_payload += bytes;
}
static void scan_generation(Gen *g, size_t instance) {
  for (Rp *p = clear_fp(g->fp); p; p = clear_tospace_bit(p->n)) {
    uintptr_t *start = (uintptr_t *)p+HEADER_WORDS_IN_REGION_PAGE;
    uintptr_t *end = (uintptr_t *)p+HEADER_WORDS_IN_REGION_PAGE+ALLOCATABLE_WORDS_IN_REGION_PAGE;
    uintptr_t *limit = clear_tospace_bit(p->n) ? end : g->a;
    while (start < limit) {
      const ObjectDesc *obj = (const ObjectDesc *)start;
      if (!obj->packed) break;
      size_t size = obj->packed & OBJECT_DESC_SIZE_MASK;
      if (size == OBJECT_DESC_SIZE_MASK || size+sizeObjectDesc > (size_t)(limit-start))
        fail("invalid object descriptor in selected region");
      count_object(instance,obj);
      start += size+sizeObjectDesc;
    }
    occupied_slack += (uint64_t)(end-start)*sizeof(uintptr_t);
  }
}
static int selected_binding(const char *unit, uint64_t id) {
  if (all_regions()) return 1;
  if (!mlkit_rp_region) return 0;
  const char *colon = strrchr(mlkit_rp_region,':');
  size_t n = (size_t)(colon-mlkit_rp_region);
  return strlen(unit) == n && !memcmp(unit,mlkit_rp_region,n) && id == strtoull(colon+1,NULL,10);
}
#endif
static void region_record(const char *unit, const char *name, const char *source, uint64_t id, uintptr_t *storage,
                          uintptr_t words, uintptr_t run_type, uint64_t *pages_visited) {
  if (words != UINTPTR_MAX) return; /* finite storage is stack storage */
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
                       record_worker,words == UINTPTR_MAX,record_cpu,run_type,g0_pages,g0_tail,pages-g0_pages,tail-g0_tail,0,0,0,0,0,0});
#ifdef PROFILING
  if (selected_binding(unit,id)) {
    selected_unit = unit; selected_name = name; selected_source = source; selected_binding_id = id;
    memset(occupancy_hash,0,sizeof(occupancy_hash));
    occupied_payload = occupied_objects = occupied_slack = 0;
    size_t instance = record_count-1;
    Region r = (Region)storage;
    scan_generation(&r->g0,instance);
#ifdef ENABLE_GEN_GC
    scan_generation(&r->g1,instance);
#endif
    for (Lobjs *p = clear_lobj_bit(r->lobjs); p; p = clear_lobj_bit(p->next))
      count_object(instance,(ObjectDesc *)&p->value);
    records[instance].payload = occupied_payload;
    records[instance].objects = occupied_objects;
    records[instance].object_overhead = occupied_objects*sizeof(ObjectDesc);
    records[instance].slack = occupied_slack;
    records[instance].detailed = 1;
  }
#endif

}
/* Binding definitions are emitted once on first observation. The native unit
 * strings live in resident code images, including retained REPL libraries. */
typedef struct Definition {
  Record metadata;
  struct Definition *next;
} Definition;
static Definition *definitions;
static uint64_t definition_count;
static const char *run_type_name(uintptr_t type);
static void define_record(Record *r) {
  for (Definition *d = definitions; d; d = d->next) {
    const Record *m = &d->metadata;
    if (m->id == r->id && m->infinite == r->infinite && m->run_type == r->run_type &&
        !strcmp(m->unit,r->unit) && !strcmp(m->source,r->source) && !strcmp(m->name,r->name)) {
      r->definition = m->definition;
      return;
    }
  }
  if (definition_count == UINT64_MAX) fail("too many binding definitions");
  r->definition = ++definition_count;
  Definition *d = checked_alloc(sizeof(*d));
  *d = (Definition){*r,definitions}; definitions = d;
  emit_record(5,NUMS(r->definition,r->id),
              STRS(r->unit,r->name,r->source,r->infinite ? "infinite" : "finite",run_type_name(r->run_type)));
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
  emit_record(7,NUMS(sequence,r->thread,(uint64_t)(int64_t)r->worker,(uint64_t)(int64_t)r->cpu,
                    r->definition,r->g0_pages,r->g0_tail,r->g1_pages,r->g1_tail,
                    r->pages,r->tail,r->pages*sizeof(Rp)-r->tail,r->big,r->finite,r->desc),NO_STRINGS);
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
/* Built-in globals use compiler region keys in both snapshot and attribution
 * profiles. Other persistent regions (e.g. REPL regions) get distinct IDs. */
typedef struct GlobalRegion {
  Region region;
  uint64_t id;
  struct GlobalRegion *next;
} GlobalRegion;
static GlobalRegion *globals;
static uint64_t next_global_id;
static uint64_t global_id(Region r) {
  uintptr_t type = global_type(r);
  if (type) return global_metadata(type)->binding;
  for (GlobalRegion *g = globals; g; g = g->next) if (g->region == r) return g->id;
  GlobalRegion *g = checked_alloc(sizeof(*g));
  *g = (GlobalRegion){r,next_global_id++ + 8,globals};
  globals = g;
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
  stacks[stack_count++] = (StackRecord){record_thread,active,0,record_worker,record_cpu};
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
  } else {
    record_cpu = current_cpu();
    walk(ctx,base,map,&frames,&pages);
  }
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
  emit_record(6,NUMS(sequence,start,request_time,sample_wait),
              STRS(mlkit_rp_gc_major < 0 ? "none" : mlkit_rp_gc_major ? "major" : "minor",
                   op == 0 ? "start" : op == 1 ? "pause" : op == 3 ? "periodic" : op == 4 ? "before_gc" : op == 5 ? "after_gc" : "explicit"));
  for (size_t i = 0; i < record_count; i++) write_record(&records[i]);
#ifdef PROFILING
  while (occupancy) {
    Occupancy *o = occupancy;
    Record *r = &records[o->instance];
    uint64_t site = allocation_definition(o->site);
    emit_record(18,NUMS(sequence,o->instance,r->definition,r->thread,site,o->count,o->bytes,(uint64_t)(int64_t)r->worker,(uint64_t)(int64_t)r->cpu),NO_STRINGS);
    occupancy = o->next; free(o);
  }
  for (size_t i = 0; i < record_count; i++) if (records[i].detailed) {
    Record *r = &records[i];
    emit_record(19,NUMS(sequence,i,r->definition,r->payload,r->objects,r->object_overhead,r->slack),NO_STRINGS);
  }
#endif

  for (size_t i = 0; i < stack_count; i++) {
    const StackRecord *r = &stacks[i];
    emit_record(8,NUMS(sequence,r->thread,(uint64_t)(int64_t)r->worker,(uint64_t)(int64_t)r->cpu,
                      r->active,r->finite,r->active-r->finite),NO_STRINGS);
  }
  emit_record(9,NUMS(sequence,timestamp(),frames,pages,cached_pages,cached_pages*sizeof(Rp),
                    atomic_load(&maximum_pages),gc_collections),NO_STRINGS);
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
        emit_record(10,NUMS(timestamp()),STRS("safe_point_timeout"));
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
  emit_record(11,NUMS(timestamp()),(const char *const[]){label->data},1,
              (const size_t[]){sizeStringDefine(label)});
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
