/*----------------------------------------------------------------*
 *                     Garbage Collection                         *
 *----------------------------------------------------------------*/

#include "Flags.h"

#ifndef GC_H
#define GC_H

#ifdef ENABLE_GC
extern size_t time_to_gc;
extern size_t rp_gc_treshold;
extern size_t *stack_bot_gc;
extern size_t alloc_period;
extern uintptr_t *data_lab_ptr;

extern size_t lobjs_current;
extern size_t lobjs_gc_treshold;
extern size_t lobjs_period;

extern ssize_t doing_gc;
#ifdef ENABLE_GEN_GC
extern ssize_t major_p;
extern ssize_t num_gc_major;
#endif
extern ssize_t num_gc;

extern ssize_t raised_exn_interupt;
extern ssize_t raised_exn_overflow;

extern ssize_t time_gc_all_ms;

extern size_t *data_begin_addr;
extern size_t *data_end_addr;

size_t size_lobj(size_t tag);

void gc(size_t **sp, size_t reg_map);

#endif /*ENABLE_GC*/

#endif /*GC_H*/
