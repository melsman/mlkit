/*----------------------------------------------------------------*
 *                     Garbage Collection                         *
 *----------------------------------------------------------------*/

#ifndef GC_H
#define GC_H

#ifdef ENABLE_GC
extern int time_to_gc;
extern unsigned int *stack_bot_gc;
extern unsigned int alloc_period;
extern int *data_lab_ptr;

extern unsigned int lobj_current;
extern unsigned int lobj_gc_treshold;

extern int doing_gc;
extern int raised_exn_interupt;
extern int raised_exn_overflow;

unsigned int size_lobj(unsigned int tag);

void gc(unsigned int **sp, unsigned int reg_map);
#endif /*ENABLE_GC*/

#endif /*GC_H*/
