#ifndef MLKIT_REGION_PROFILE_H
#define MLKIT_REGION_PROFILE_H
#include <stdint.h>
#include <stddef.h>
#include <signal.h>
#include <stdatomic.h>
#include "Region.h"

/* Version 3 native map, read backwards from its end/return PC (64-bit words):
 * magic, return-slot offset from frame base, caller-base delta from return
 * slot, binding count, PC-relative ML unit-name string, then (id, offset, size, relative name, run type) quintuples.
 * Size UINTPTR_MAX denotes an infinite region; return offset UINTPTR_MAX
 * terminates an ML entry; UINTPTR_MAX-1 marks an unsupported C callback boundary. Unit and name offsets
 * are relative to their own words; zero means no explicit name. Other offsets
 * and sizes are in machine words. */
#define MLKIT_RP_MAGIC UINT64_C(0x52504d33)
extern const uintptr_t mlkit_rp_capable;
extern int mlkit_rp_enabled;
extern _Atomic int mlkit_rp_pending;
extern uint64_t mlkit_rp_interval_us;
extern int mlkit_rp_report;
extern int mlkit_rp_gc_samples;
extern int mlkit_rp_gc_major;
int mlkit_rp_parse_interval(const char *);
uintptr_t mlkit_rp_poll(Context, uintptr_t *, const uintptr_t *);
extern int mlkit_rp_initially_paused;
extern const char *mlkit_rp_filename;
extern const char *mlkit_rp_control;
void mlkit_rp_init(void);
void mlkit_rp_thread_create(Context, int);
void mlkit_rp_thread_enter(Context);
void mlkit_rp_thread_exit(Context);
uintptr_t mlkit_rp_wait_enter(Context, uintptr_t *, const uintptr_t *);
uintptr_t mlkit_rp_wait_leave(Context);
void mlkit_rp_close(void);
void mlkit_rp_page_alloc(void);
void mlkit_rp_pages_free(Rp *);
void mlkit_rp_idle(Context);
void mlkit_rp_large_alloc(void *, size_t);
void mlkit_rp_large_free(void *);
uintptr_t mlkit_rp_capture(Context, uintptr_t *, const uintptr_t *, uintptr_t);
uintptr_t mlkit_rp_start(void);
uintptr_t mlkit_rp_pause(void);
uintptr_t mlkit_rp_sample(void);
uintptr_t mlkit_rp_flush(void);
struct stringDesc;
uintptr_t mlkit_rp_mark(struct stringDesc *);
/* A combined continuation keeps the collector bitmap before the profiler map.
 * Entry/callback sentinels have no bindings or collector bitmap. */
static inline const uintptr_t *mlkit_rp_gc_map(const uintptr_t *fd) {
  static const uintptr_t end[] = {UINTPTR_MAX,0,0};
  if (fd[-1] != MLKIT_RP_MAGIC) return fd;
  if (fd[-2] >= UINTPTR_MAX-1) return end+3;
  return fd-5-5*fd[-4];
}
#endif
