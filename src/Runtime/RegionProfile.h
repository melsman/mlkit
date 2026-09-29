#ifndef MLKIT_REGION_PROFILE_H
#define MLKIT_REGION_PROFILE_H
#include <stdint.h>
#include <stddef.h>
#include "Region.h"

/* Version 1 native map, read backwards from its end/return PC (64-bit words):
 * magic, return-slot offset from frame base, caller-base delta from return
 * slot, binding count, PC-relative ML unit-name string, then (id, offset, size) triples.
 * Size UINTPTR_MAX denotes an infinite region; return offset UINTPTR_MAX
 * terminates an ML entry; UINTPTR_MAX-1 rejects a C callback. The unit offset is relative to its own word. Offsets and sizes are in machine words. */
#define MLKIT_RP_MAGIC UINT64_C(0x52504d31)
extern const uintptr_t mlkit_rp_capable;
extern int mlkit_rp_enabled;
extern int mlkit_rp_initially_paused;
extern const char *mlkit_rp_filename;
void mlkit_rp_init(void);
void mlkit_rp_close(void);
void mlkit_rp_large_alloc(void *, size_t);
void mlkit_rp_large_free(void *);
uintptr_t mlkit_rp_capture(Context, uintptr_t *, const uintptr_t *, uintptr_t);
uintptr_t mlkit_rp_start(void);
uintptr_t mlkit_rp_pause(void);
uintptr_t mlkit_rp_sample(void);
uintptr_t mlkit_rp_flush(void);
struct stringDesc;
uintptr_t mlkit_rp_mark(struct stringDesc *);
#endif
