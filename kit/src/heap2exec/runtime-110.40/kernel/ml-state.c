/* ml-state.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 */

#include <stdarg.h>
#include "ml-base.h"
#include "vproc-state.h"
#include "ml-state.h"
#include "system-signals.h"
#include "tags.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-globals.h"
#include "gc.h"
#include "ml-timer.h"
#include "ml-limits.h"


vproc_state_t	*VProc[MAX_NUM_PROCS];
int		NumVProcs;


/* local routines */
PVT void InitVProcState (vproc_state_t *vsp);


/* AllocMLState:
 */
ml_state_t *AllocMLState (bool_t isBoot, heap_params_t *heapParams)
{
    ml_state_t	*msp = NIL(ml_state_t *);
#ifdef MP_SUPPORT
    int		i;
#endif

#ifdef MP_SUPPORT

    for (i = 0; i < MAX_NUM_PROCS; i++) {
	if (((VProc[i] = NEW_OBJ(vproc_state_t)) == NIL(vproc_state_t *))
	||  ((msp = NEW_OBJ(ml_state_t)) == NIL(ml_state_t *))) {
	    Die ("unable to allocate ML state vectors");
	}
	VProc[i]->vp_state = msp;
    }
    msp = VProc[0]->vp_state;
#else
    if (((VProc[0] = NEW_OBJ(vproc_state_t)) == NIL (vproc_state_t *))
    ||  ((msp = NEW_OBJ(ml_state_t)) == NIL(ml_state_t *))) {
	Die ("unable to allocate ML state vector");
    }
    VProc[0]->vp_state = msp;
#endif /* MP_SUPPORT */

  /* allocate and initialize the heap data structures */
    InitHeap (msp, isBoot, heapParams);

#ifdef MP_SUPPORT
  /* partition the allocation arena given by InitHeap among the
   * MAX_NUM_PROCS processors.
   */
    NumVProcs = MAX_NUM_PROCS;
    PartitionAllocArena(VProc);
  /* initialize the per-processor ML state */
    for (i = 0; i < MAX_NUM_PROCS; i++) {
	int	j;

	InitVProcState (VProc[i]);
      /* single timers are currently shared among multiple processors */
	if (i != 0) {
	    VProc[i]->vp_gcTime0 = VProc[0]->vp_gcTime0;
	    VProc[i]->vp_gcTime	= VProc[0]->vp_gcTime;
	}
    }
  /* initialize the first processor here */
    VProc[0]->vp_mpSelf = MP_ProcId ();
    VProc[0]->vp_mpState = MP_PROC_RUNNING;
#else
    InitVProcState (VProc[0]);
    NumVProcs = 1;
#endif /* MP_SUPPORT */

  /* initialize the timers */
  /** MP_SUPPORT note: for now, only proc 0 has timers **/
    ResetTimers (VProc[0]);

    return msp;

} /* end of AllocMLState */

/* InitVProcState:
 */
PVT void InitVProcState (vproc_state_t *vsp)
{
    int		i;

    vsp->vp_heap		= vsp->vp_state->ml_heap;
    vsp->vp_state->ml_vproc	= vsp;
    vsp->vp_inMLFlag		= FALSE;
    vsp->vp_handlerPending	= FALSE;
    vsp->vp_inSigHandler	= FALSE;
    vsp->vp_numPendingSysSigs	= 0;
    vsp->vp_numPendingSigs	= 0;
    vsp->vp_sigCode		= 0;
    vsp->vp_sigCount		= 0;
    vsp->vp_nextPendingSig	= 0;
    vsp->vp_numInQ		= 0;
    vsp->vp_gcSigState		= ML_SIG_IGNORE;
    vsp->vp_gcTime0		= NEW_OBJ(Time_t);
    vsp->vp_gcTime		= NEW_OBJ(Time_t);

    for (i = 0;  i < NUM_SIGS;  i++) {
	vsp->vp_pendingSigQ[i].sigNum = 0;
	vsp->vp_pendingSigQ[i].count = 0;
    }

  /* initialize the ML state, including the roots */
    InitMLState (vsp->vp_state);
    vsp->vp_state->ml_arg		= ML_unit;
    vsp->vp_state->ml_cont		= ML_unit;
    vsp->vp_state->ml_closure		= ML_unit;
    vsp->vp_state->ml_linkReg		= ML_unit;
    vsp->vp_state->ml_pc		= ML_unit;
    vsp->vp_state->ml_exnCont		= ML_unit;
    vsp->vp_state->ml_varReg		= ML_unit;
    vsp->vp_state->ml_calleeSave[0]	= ML_unit;
    vsp->vp_state->ml_calleeSave[1]	= ML_unit;
    vsp->vp_state->ml_calleeSave[2]	= ML_unit;

#ifdef MP_SUPPORT
    vsp->vp_mpSelf		= 0;
    vsp->vp_mpState		= MP_PROC_NO_PROC;
#endif

} /* end of InitVProcState */

/* InitMLState:
 *
 * Initialize the ML State vector.  Note that we do not initialize the root
 * registers here, since this is sometimes called when the roots are live (from
 * ML_ApplyFn).
 */
void InitMLState (ml_state_t *msp)
{
    msp->ml_storePtr		= ML_unit;
#ifdef SOFT_POLL
    msp->ml_pollPending		= FALSE;
    msp->ml_inPollHandler	= FALSE;
#endif

} /* end of InitMLState. */

/* SaveCState:
 *
 *    Build a return closure that will save a collection of ML values
 * being used by C.  The ML values are passed by reference, with NIL
 * as termination.
 */
void SaveCState (ml_state_t *msp, ...)
{
    va_list	    ap;
    int		    n, i;
    ml_val_t	    *vp;

    va_start (ap, msp);
    for (n = 0; (vp = va_arg(ap, ml_val_t *)) != NIL(ml_val_t *);  n++)
	continue;
    va_end (ap);

    va_start (ap, msp);
    ML_AllocWrite (msp, 0, MAKE_DESC(n, DTAG_record));
    for (i = 1;  i <= n;  i++) {
	vp = va_arg (ap, ml_val_t *);
        ML_AllocWrite (msp, i, *vp);
    }
    msp->ml_calleeSave[0]   = ML_Alloc(msp, n);
    msp->ml_cont	    = PTR_CtoML(return_c);
    va_end (ap);

} /* end of SaveCState */

/* RestoreCState:
 *
 *    Restore a collection of ML values from the return closure.
 */
void RestoreCState (ml_state_t *msp, ...)
{
    va_list	ap;
    int		n, i;
    ml_val_t	*vp;
    ml_val_t	savedState;

    va_start (ap, msp);
    savedState = msp->ml_calleeSave[0];
    n = OBJ_LEN(savedState);
    for (i = 0;  i < n;  i++) {
	vp = va_arg (ap, ml_val_t *);
	*vp = REC_SEL(savedState, i);
    }
    va_end (ap);

} /* end of RestoreCState */

