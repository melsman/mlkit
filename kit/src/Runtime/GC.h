/*----------------------------------------------------------------*
 *                     Garbage Collection                         *
 *----------------------------------------------------------------*/

#ifndef GC
#define GC

extern int time_to_gc;
extern int *stack_bot_gc;
extern int alloc_period;
extern int *data_lab_ptr;

extern int doing_gc;
extern int raised_exn_interupt;
extern int raised_exn_overflow ;

#ifdef ENABLE_GC

void gc(unsigned int sp, unsigned int reg_map);
#endif /* ENABLE_GC */

#endif /*GC*/
