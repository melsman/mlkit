/* signal-util.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 *
 * System independent utility routines for supporting signals and
 * software polling.
 */

#include <stdio.h>
#include "ml-base.h"
#include "ml-limits.h"
#include "ml-state.h"
#include "vproc-state.h"
#include "ml-objects.h"
#include "ml-signals.h"
#include "system-signals.h"


/* ChooseSignal:
 *
 * Choose which signal to pass to the ML handler and setup the ML state
 * vector accordingly.
 * WARNING: This should be called with signals masked to avoid race
 * conditions.
 */
void ChooseSignal (vproc_state_t *vsp)
{
    ASSERT(vsp->vp_numInQ > 0);

  /* record the signal and count */
    vsp->vp_sigCode = vsp->vp_pendingSigQ[vsp->vp_nextPendingSig].sigNum;
    vsp->vp_sigCount = vsp->vp_pendingSigQ[vsp->vp_nextPendingSig].count;
    if (IS_SYSTEM_SIG(vsp->vp_sigCode))
	vsp->vp_numPendingSysSigs -= vsp->vp_sigCount;
    else
        vsp->vp_numPendingSigs -= vsp->vp_sigCount;

  /* advance the pending queue */
    if ((--vsp->vp_numInQ == 0) || (++vsp->vp_nextPendingSig == NUM_SIGS))
	vsp->vp_nextPendingSig = 0;

#ifdef SIGNAL_DEBUG
SayDebug ("ChooseSignal: sig = %d, count = %d\n",
vsp->vp_sigCode, vsp->vp_sigCount);
#endif

} /* end of ChooseSignal */


/* EnqueueSignal:
 *
 * Add a signal to the pending queue; if the signal is already present, then
 * bump its count.
 */
void EnqueueSignal (vproc_state_t *vsp, int sigCode)
{
    int		i, j;

#ifdef SIGNAL_DEBUG
SayDebug("EnqueueSignal: numInQ = %d, sig = %d\n", vsp->vp_numInQ, sigCode);
#endif

    ASSERT(vsp->vp_numInQ >= 0);

    for (i = vsp->vp_nextPendingSig, j = vsp->vp_numInQ;  --j >= 0; ) {
	if (vsp->vp_pendingSigQ[i].sigNum == sigCode)
	    break;
	if (++i == NUM_SIGS) i = 0;
    }
    if (j < 0) {
	vsp->vp_pendingSigQ[i].sigNum = sigCode;
	vsp->vp_pendingSigQ[i].count = 1;
	vsp->vp_numInQ++;
    }
    else
	vsp->vp_pendingSigQ[i].count++;

} /* end of EnqueueSignal */


/* MakeResumeCont:
 *
 * Build the resume continuation for a signal or poll event handler.
 * This closure contains the address of the resume entry-point and
 * the registers from the ML State.
 *
 * At least 4K avail. heap assumed.
 */
ml_val_t MakeResumeCont (ml_state_t *msp, ml_val_t resume[])
{
  /* allocate the resumption closure */
    ML_AllocWrite(msp,  0, MAKE_DESC(10, DTAG_record));
    ML_AllocWrite(msp,  1, PTR_CtoML(resume));
    ML_AllocWrite(msp,  2, msp->ml_arg);
    ML_AllocWrite(msp,  3, msp->ml_cont);
    ML_AllocWrite(msp,  4, msp->ml_closure);
    ML_AllocWrite(msp,  5, msp->ml_linkReg);
    ML_AllocWrite(msp,  6, msp->ml_pc);
    ML_AllocWrite(msp,  7, msp->ml_exnCont);
    /* John (Reppy) says that varReg should not be included here...
    ML_AllocWrite(msp,  8, msp->ml_varReg);
    */
    ML_AllocWrite(msp,  8, msp->ml_calleeSave[0]);
    ML_AllocWrite(msp,  9, msp->ml_calleeSave[1]);
    ML_AllocWrite(msp, 10, msp->ml_calleeSave[2]);

    return ML_Alloc(msp, 10);

} /* end of MakeResumeCont */


/* MakeHandlerArg:
 *
 * Build the argument record for the ML signal handler.  It has the type
 *
 *   val sigHandler : (int * int * unit cont) -> 'a
 *
 * The first argument is the signal code, the second is the signal count and the
 * third is the resumption continuation.  The ML signal handler should never
 * return.
 * NOTE: maybe this should be combined with ChooseSignal???
 */
ml_val_t MakeHandlerArg (ml_state_t *msp, ml_val_t resume[])
{
    ml_val_t	resumeCont, arg;
    vproc_state_t *vsp = msp->ml_vproc;

    resumeCont = MakeResumeCont(msp, resume);

  /* allocate the ML signal handler's argument record */
    REC_ALLOC3(msp, arg,
	INT_CtoML(vsp->vp_sigCode), INT_CtoML(vsp->vp_sigCount),
	resumeCont);

#ifdef SIGNAL_DEBUG
SayDebug ("MakeHandlerArg: resumeC = %#x, arg = %#x\n", resumeCont, arg);
#endif
    return arg;

} /* end of MakeHandlerArg */


/* LoadResumeState:
 *
 * Load the ML state with the state preserved in resumption continuation
 * made by MakeResumeCont.
 */
void LoadResumeState (ml_state_t *msp)
{
    ml_val_t	    *contClosure;
#ifdef SIGNAL_DEBUG
SayDebug ("LoadResumeState:\n");
#endif

    contClosure = PTR_MLtoC(ml_val_t, msp->ml_closure);

    msp->ml_arg			= contClosure[1];
    msp->ml_cont		= contClosure[2];
    msp->ml_closure		= contClosure[3];
    msp->ml_linkReg		= contClosure[4];
    msp->ml_pc			= contClosure[5];
    msp->ml_exnCont		= contClosure[6];
    /* John says ...
    msp->ml_varReg		= contClosure[7];
    */
    msp->ml_calleeSave[0]	= contClosure[7];
    msp->ml_calleeSave[1]	= contClosure[8];
    msp->ml_calleeSave[2]	= contClosure[9];

} /* end of LoadResumeState */


/* GCSignal:
 *
 * Record a garbage collection signal (if enabled).  Return true, if a signal
 * was recorded.
 */
bool_t GCSignal (vproc_state_t *vsp)
{
    if (vsp->vp_gcSigState == ML_SIG_ENABLED) {
	vsp->vp_numPendingSigs++;
	EnqueueSignal (vsp, RUNSIG_GC);
	return TRUE;
    }
    else
	return FALSE;

} /* end of GCSignal */

