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
        vsp->vp_numPendingSigs -= vsp->vp_sigCount;
    else
	vsp->vp_numPendingSysSigs -= vsp->vp_sigCount;

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
 * Layout of the resumption continuation:
 *
 *                  resumption continuation
 *                            |
 *                            v
 *   +------------------+----+-+-+-+-+-+~+---------~
 *   |STRING| floatregs |DESC|1|2|3|4| |B| live regs
 *   +------------------+----+-+-+-+-+|+~+---------~
 *           ^                        |
 *           |________________________|
 *
 * At least 4K avail. heap assumed.
 */
ml_val_t MakeResumeCont (ml_state_t *msp, ml_val_t resume[])
{
    ml_val_t	fpRegs;
    int		i, n, mask;

#if (FLOAT_CALLEESAVE > 0)
#  ifdef ALIGN_REALDS
  /* Force REALD_SZB alignment */
    msp->ml_allocPtr =
	(ml_val_t *)(((Addr_t)(msp->ml_allocPtr) & ~(REALD_SZB-1))+WORD_SZB);
#  endif
#  if defined(TARGET_X86)
    n = FP_STATE_SIZE;
#  else
    n = (REALD_SZB*FLOAT_CALLEESAVE)/WORD_SZB;
    ML_AllocWrite(msp, 0, MAKE_DESC(n, DTAG_string));
    SaveFPRegs ((Addr_t)(msp->ml_allocPtr) + WORD_SZB);
    fpRegs = ML_Alloc(msp, n);
#  endif /* TARGET_X86 */
#else
    fpRegs = ML_unit;
#endif

  /* allocate the resumption closure */
    ML_AllocWrite(msp, 1, PTR_CtoML(resume));
    ML_AllocWrite(msp, 2, INT_CtoML(msp->ml_liveRegMask));
    ML_AllocWrite(msp, 3, msp->ml_pc);
    ML_AllocWrite(msp, 4, msp->ml_exnCont);
    ML_AllocWrite(msp, 5, fpRegs);
    n = 6;

#if  defined(BASE_INDX)
    ML_AllocWrite(msp, n, msp->ml_baseReg);
    n++;
#endif

  /* save the live registers */
    mask = msp->ml_liveRegMask;
    for (i = 0;  mask != 0;  i++, mask >>= 1) {
	if (mask & 0x1) {
	    ML_AllocWrite(msp, n, msp->ml_roots[ArgRegMap[i]]);
	    n++;
	}
    }

 /* write the object descriptor */
    ML_AllocWrite(msp, 0, MAKE_DESC(n-1, DTAG_record));

    return ML_Alloc(msp, n-1);

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
    int		    i, n, mask;
#ifdef SIGNAL_DEBUG
SayDebug ("LoadResumeState:\n");
#endif

    contClosure = PTR_MLtoC(ml_val_t, msp->ml_closure);

    mask		=
    msp->ml_liveRegMask	= INT_MLtoC(contClosure[1]);
    msp->ml_pc		= contClosure[2];
    msp->ml_exnCont	= contClosure[3];
#if (FLOAT_CALLEESAVE > 0)
    RestoreFPRegs(PTR_MLtoC(Word_t, contClosure[4]));
#endif
    n = 5;
#ifdef BASE_INDX
    msp->ml_baseReg	= contClosure[n];
    n++;
#endif
    for (i = 0;  mask != 0;  i++, mask >>= 1) {
	if (mask & 0x1) {
	    msp->ml_roots[ArgRegMap[i]] = contClosure[n];
	    n++;
	}
    }

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

