/* trace.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 *
 * Trace facility for the bytecode interpreter.
 */

#include "ml-base.h"
#include "ml-values.h"

#ifdef TARGET_BYTECODE
#  include "memory-trace.h"
#endif

#ifdef INSTR_TRACE
extern bool_t		traceOn;
#endif

#ifdef FULL_HIST
extern bool_t       fullHistOn;
#endif

/* ml_start_trace:
 */
ml_val_t ml_start_trace (ml_state_t *msp, ml_val_t *arg)
{
#if defined(TARGET_BYTECODE)
#ifdef FULL_HIST
if (fullHistOn == FALSE) printf("*** TURNING FULL_HIST ON ***\n");
else printf("*** START\n");
    fullHistOn = TRUE;
#endif
#ifdef INSTR_TRACE
    traceOn = TRUE;
#endif
#ifdef DO_MEMORY_TRACE
    MemOp_Start (msp);
#endif
#endif

} /* end of ml_start_trace */

/* ml_stop_trace:
 */
ml_val_t ml_stop_trace (ml_state_t *msp, ml_val_t *arg)
{
#if defined(TARGET_BYTECODE)
#ifdef INSTR_TRACE
    traceOn = FALSE;
#endif
#ifdef DO_MEMORY_TRACE
    MemOp_Stop (msp);
#endif
#endif

} /* end of ml_stop_trace */

