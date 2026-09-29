/* Explicit, single-threaded region snapshots. No object/page-content scan. */
#include "RegionProfile.h"
#include "String.h"
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <time.h>
#include <string.h>

__attribute__((weak)) const uintptr_t mlkit_rp_capable = 0;
int mlkit_rp_enabled;
int mlkit_rp_initially_paused;
const char *mlkit_rp_filename = "profile.rp";
static FILE *output;
static int active;
static uint64_t sequence;
static struct timespec origin;

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
  exit(EXIT_FAILURE);
}
static void *checked_alloc(size_t n) {
  void *p = malloc(n);
  if (!p) fail("out of memory");
  return p;
}
void mlkit_rp_large_alloc(void *p, size_t words) {
  if (!mlkit_rp_enabled) return;
  Large *entry = checked_alloc(sizeof(*entry));
  entry->address = p;
  entry->bytes = (uint64_t)words * sizeof(uintptr_t);
  size_t h = bucket(p);
  entry->next = large[h];
  large[h] = entry;
}
void mlkit_rp_large_free(void *p) {
  if (!mlkit_rp_enabled) return;
  Large **link = &large[bucket(p)];
  while (*link && (*link)->address != p) link = &(*link)->next;
  if (!*link) fail("large object missing from size table");
  Large *entry = *link;
  *link = entry->next;
  free(entry);
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
static void quoted_bytes(const char *s, size_t n) {
  fputc('"', output);
  for (const unsigned char *p = (const unsigned char *)s; n; p++, n--) {
    if (*p == '"' || *p == '\\') fprintf(output, "\\%c", *p);
    else if (*p < 32 || *p >= 127) fprintf(output, "\\u%04x", *p);
    else fputc(*p, output);
  }
  fputc('"', output);
}
void mlkit_rp_close(void) {
  if (!output) return;
  FILE *f = output;
  output = NULL;
  if (fclose(f)) fail("cannot close profile output");
}
void mlkit_rp_init(void) {
  if (!mlkit_rp_enabled) return;
#if defined(ENABLE_GC) || defined(PARALLEL) || defined(PROFILING)
  fail("M1 requires the ordinary single-threaded no-GC runtime");
#endif
  if (mlkit_rp_capable != MLKIT_RP_MAGIC)
    fail("recompile the executable and its ML libraries with -region_profile -no_gc");
  output = fopen(mlkit_rp_filename, "w");
  if (!output) fail("cannot open profile output");
  if (clock_gettime(CLOCK_MONOTONIC, &origin)) fail("cannot read clock");
  active = !mlkit_rp_initially_paused;
  fprintf(output, "{\"type\":\"header\",\"format\":\"mlkit-region-profile\",\"version\":1,\"time_unit\":\"ns\",\"size_unit\":\"bytes\",\"word_bytes\":%zu,\"page_bytes\":%zu}\n",
          sizeof(uintptr_t), sizeof(Rp));
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
static void region_record(const char *unit, uint64_t id, uintptr_t *storage,
                          uintptr_t words, uint64_t *pages_visited) {
  uint64_t pages = 0, tail = 0, big = 0, finite = 0, desc = 0;
  if (words == UINTPTR_MAX) {
    Region r = (Region)storage;
    if (!remember(r)) return;
    for (Rp *p = clear_fp(r->g0.fp); p; p = clear_tospace_bit(p->n)) pages++;
    tail = (uint64_t)(rpBoundary(r->g0.a)-r->g0.a)*sizeof(uintptr_t);
    for (Lobjs *p = r->lobjs; p; p = clear_lobj_bit(p->next)) big += large_size(p);
    desc = sizeof(Ro);
  } else finite = (uint64_t)words*sizeof(uintptr_t);
  *pages_visited += pages;
  fprintf(output, "{\"type\":\"region\",\"sample\":%" PRIu64 ",\"thread\":0,\"unit\":", sequence);
  quoted_bytes(unit, strlen(unit));
  fprintf(output, ",\"binding\":%" PRIu64 ",\"kind\":\"%s\",\"pages\":%" PRIu64
          ",\"unused_tail\":%" PRIu64 ",\"page_footprint\":%" PRIu64
          ",\"large_bytes\":%" PRIu64 ",\"finite_bytes\":%" PRIu64
          ",\"descriptor_bytes\":%" PRIu64 "}\n", id,
          words == UINTPTR_MAX ? "infinite" : "finite", pages, tail,
          pages*sizeof(Rp)-tail, big, finite, desc);
}
uintptr_t mlkit_rp_capture(Context ctx, uintptr_t *base, const uintptr_t *map, uintptr_t op) {
  if (!mlkit_rp_enabled) return 1;
  if (op == 0 && active) return 1;
  if (op == 1 && !active) return 1;
  uint64_t start = timestamp(), frames = 0, pages = 0;
  sequence++;
  seen_count = 0;
  fprintf(output, "{\"type\":\"sample_begin\",\"sample\":%" PRIu64 ",\"time\":%" PRIu64
          ",\"reason\":\"%s\"}\n", sequence, start, op == 0 ? "start" : op == 1 ? "pause" : "explicit");
  for (;;) {
    if (map[-1] != MLKIT_RP_MAGIC) fail("missing or incompatible ML frame metadata");
    if (map[-2] == UINTPTR_MAX) break;
    if (map[-2] == UINTPTR_MAX-1) fail("M1 cannot sample across a C-to-ML callback boundary");
    if (++frames > 1000000 || map[-4] > 1000000) fail("invalid frame metadata");
    const char *unit = ((String)((uintptr_t)(map-5)+map[-5]))->data;
    for (uintptr_t i = 0; i < map[-4]; i++) {
      const uintptr_t *entry = map-6-3*i;
      region_record(unit, entry[0], base+entry[-1], entry[-2], &pages);
    }
    uintptr_t *ret = base+map[-2];
    map = (const uintptr_t *)*ret;
    if (map[-1] != MLKIT_RP_MAGIC) fail("missing caller metadata: rebuild all ML libraries with -region_profile");
    if (map[-2] == UINTPTR_MAX) break;
    if (map[-2] == UINTPTR_MAX-1) fail("M1 cannot sample across a C-to-ML callback boundary");
    uintptr_t *parent = ret+map[-3];
    if (parent <= base) fail("non-increasing ML frame chain");
    base = parent;
  }
  /* Global regions outlive all compilation-unit calls and have no ML frame.
   * Locals already recorded through maps are deduplicated here. */
  uint64_t global_id = 0;
  for (Region r = ctx->topregion; r; r = r->p) {
    size_t i;
    for (i = 0; i < seen_count && seen[i] != r; i++) {}
    if (i == seen_count)
      region_record("<global>", global_id++, (uintptr_t *)r, UINTPTR_MAX, &pages);
  }
  fprintf(output, "{\"type\":\"sample_end\",\"sample\":%" PRIu64
          ",\"time\":%" PRIu64 ",\"frames\":%" PRIu64 ",\"pages_visited\":%" PRIu64 "}\n",
          sequence, timestamp(), frames, pages);
  if (ferror(output)) fail("cannot write profile output");
  if (op == 0) active = 1;
  if (op == 1) active = 0;
  return 1;
}
/* Non-instrumented compilation remains usable with profiling disabled. */
uintptr_t mlkit_rp_start(void) { if (mlkit_rp_enabled) fail("start called from code without profiling metadata"); return 1; }
uintptr_t mlkit_rp_pause(void) { if (mlkit_rp_enabled) fail("pause called from code without profiling metadata"); return 1; }
uintptr_t mlkit_rp_sample(void) { if (mlkit_rp_enabled) fail("sample called from code without profiling metadata"); return 1; }
uintptr_t mlkit_rp_flush(void) {
  if (output && fflush(output)) fail("cannot flush profile output");
  return 1;
}
uintptr_t mlkit_rp_mark(String label) {
  if (!output) return 1;
  fprintf(output, "{\"type\":\"mark\",\"time\":%" PRIu64 ",\"label\":", timestamp());
  quoted_bytes(label->data, sizeStringDefine(label));
  fputs("}\n", output);
  if (ferror(output)) fail("cannot write profile output");
  return 1;
}
