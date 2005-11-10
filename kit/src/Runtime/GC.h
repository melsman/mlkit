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
#ifdef ENABLE_GEN_GC
extern int major_p;
#endif

extern int raised_exn_interupt;
extern int raised_exn_overflow;

extern int time_gc_all_ms;

extern unsigned int *data_begin_addr;
extern unsigned int *data_end_addr;

unsigned int size_lobj(unsigned int tag);

void gc(unsigned int **sp, unsigned int reg_map);

#endif /*ENABLE_GC*/

#endif /*GC_H*/
