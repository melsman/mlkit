/*----------------------------------------------------------------*
 *                     Garbage Collection                         *
 *----------------------------------------------------------------*/

#include "Flags.h"

#ifndef GC_H
#define GC_H

#ifdef ENABLE_GC
extern int time_to_gc;
extern unsigned int rp_gc_treshold;
extern unsigned int *stack_bot_gc;
extern unsigned int alloc_period;
extern int *data_lab_ptr;

extern unsigned int lobjs_current;
extern unsigned int lobjs_gc_treshold;
extern unsigned int lobjs_period;

extern int doing_gc;
extern int raised_exn_interupt;
extern int raised_exn_overflow;

extern unsigned int *data_begin_addr;
extern unsigned int *data_end_addr;

unsigned int size_lobj(unsigned int tag);

void gc(unsigned int **sp, unsigned int reg_map);

#ifdef SIMPLE_MEMPROF
extern int stack_min;
extern int lobjs_max_used;
extern int rp_really_used;
extern int rp_max_used;
#endif /*SIMPLE_MEMPROF*/

#endif /*ENABLE_GC*/

#endif /*GC_H*/
