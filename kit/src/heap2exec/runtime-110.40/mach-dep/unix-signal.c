/* unix-signal.c
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 *
 * Unix specific code to support ML signals.
 */

#include "ml-unixdep.h"
#include "signal-sysdep.h"
#include "ml-base.h"
#include "ml-limits.h"
#include "ml-state.h"
#include "vproc-state.h"
#include "ml-objects.h"
#include "ml-signals.h"
#include "system-signals.h"
#include "ml-globals.h"


/* The generated sys_const_t table for UNIX signals */
#include "unix-sigtbl.c"


#ifndef MP_SUPPORT
#define SELF_VPROC	(VProc[0])
#else
/** for MP_SUPPORT, we'll use SELF_VPROC for now **/
#define SELF_VPROC	(VProc[0])
#endif


#ifdef USE_ZERO_LIMIT_PTR_FN
Addr_t		SavedPC;
extern		ZeroLimitPtr[];
#endif

/* local routines */
PVT SigReturn_t CSigHandler (/* int sig, SigInfo_t info, SigContext_t *scp */);


/* ListSignals:
 */
ml_val_t ListSignals (ml_state_t *msp)
{
    return ML_SysConstList (msp, &SigTbl);

} /* end of ListSignals. */

/* PauseUntilSignal:
 *
 * Suspend the given VProc until a signal is received.
 */
void PauseUntilSignal (vproc_state_t *vsp)
{
    pause ();

} /* end of PauseUntilSignal */

/* SetSignalState:
 */
void SetSignalState (vproc_state_t *vsp, int sigNum, int sigState)
{
/* QUESTIONS:
 * If we disable a signal that has pending signals, should the pending
 * signals be discarded?
 * How do we keep track of the state of non-UNIX signals (e.g., GC).
 */
    switch (sigNum) {
      case RUNSIG_GC:
	vsp->vp_gcSigState = sigState;
	break;
      default:
	if (IS_SYSTEM_SIG(sigNum)) {
	    switch (sigState) {
	      case ML_SIG_IGNORE:
		SIG_SetHandler (sigNum, SIG_IGN);
		break;
	      case ML_SIG_DEFAULT:
		SIG_SetHandler (sigNum, SIG_DFL);
		break;
	      case ML_SIG_ENABLED:
		SIG_SetHandler (sigNum, CSigHandler);
		break;
	      default:
		Die ("bogus signal state: sig = %d, state = %d\n",
		    sigNum, sigState);
	    } /* end switch */
	}
	else Die ("SetSignalState: unknown signal %d\n", sigNum);
	    
    } /* end of switch */

} /* end of SetSignalState */


/* GetSignalState:
 */
int GetSignalState (vproc_state_t *vsp, int sigNum)
{
    switch (sigNum) {
      case RUNSIG_GC:
	return vsp->vp_gcSigState;
      default:
	if (IS_SYSTEM_SIG(sigNum)) {
	    SigReturn_t		(*handler)();
	    SIG_GetHandler (sigNum, handler);
	    if (handler == SIG_IGN)
		return ML_SIG_IGNORE;
	    else if (handler == SIG_DFL)
		return ML_SIG_DEFAULT;
	    else
		return ML_SIG_ENABLED;
	}
	else Die ("GetSignalState: unknown signal %d\n", sigNum);
    }

} /* end of GetSignalState. */


/* CSigHandler:
 *
 * The C signal handler for signals that are to be passed to the ML handler.
 */
PVT SigReturn_t CSigHandler (
    int		    sig,
#if (defined(TARGET_X86) && defined(OPSYS_LINUX))
    SigContext_t    sc)
#elif (defined(TARGET_PPC) && defined(OPSYS_LINUX))
    SigContext_t    *scp)
#else
    SigInfo_t	    info,
    SigContext_t    *scp)
#endif
{
#if defined(OPSYS_LINUX) && defined(TARGET_X86) && defined(USE_ZERO_LIMIT_PTR_FN)
    SigContext_t    *scp = &sc;
#endif
    vproc_state_t   *vsp = SELF_VPROC;

    EnqueueSignal(vsp, sig);
    vsp->vp_numPendingSysSigs++;

    /* The following line is needed only when currently executing
     * "pure" C code.  But doing it anyway in all other cases will
     * not hurt... */
    vsp->vp_limitPtrMask = 0;

    if (vsp->vp_inMLFlag && (! vsp->vp_handlerPending) && (! vsp->vp_inSigHandler)) {
	vsp->vp_handlerPending = TRUE;
#ifdef USE_ZERO_LIMIT_PTR_FN
	SIG_SavePC(vsp->vp_state, scp);
	SIG_SetPC(scp, ZeroLimitPtr);
#else /* we can adjust the heap limit directly */
	SIG_ZeroLimitPtr(scp);
#endif
    }

} /* end of CSigHandler */


/* SetSignalMask:
 *
 * Set the signal mask to the given list of signals.  The sigList has the
 * type: "sysconst list option", with the following semantics (see
 * sml-nj/boot/smlnj/signals.sml):
 *	NONE	-- the empty mask
 *	SOME[]	-- mask all signals
 *	SOME l	-- the signals in l are the mask
 */
void SetSignalMask (ml_val_t sigList)
{
    SigMask_t	mask;
    int		i;

    SIG_ClearMask(mask);

    if (sigList != OPTION_NONE) {
	sigList = OPTION_get(sigList);
	if (LIST_isNull(sigList)) {
	  /* SOME[] -- mask all signals */
	    for (i = 0;  i < NUM_SYSTEM_SIGS;  i++) {
		SIG_AddToMask(mask, SigInfo[i].id);
	    }
	}
	else {
	    while (sigList != LIST_nil) {
		ml_val_t	car = LIST_hd(sigList);
		int		sig = REC_SELINT(car, 0);
		SIG_AddToMask(mask, sig);
		sigList = LIST_tl(sigList);
	    }
	}
    }

    SIG_SetMask(mask);

} /* end of SetSignalMask */


/* GetSignalMask:
 *
 * Return the current signal mask (only those signals supported my ML); like
 * SetSignalMask, the result has the following semantics:
 *	NONE	-- the empty mask
 *	SOME[]	-- mask all signals
 *	SOME l	-- the signals in l are the mask
 */
ml_val_t GetSignalMask (ml_state_t *msp)
{
    SigMask_t	mask;
    ml_val_t	name, sig, sigList, res;
    int		i, n;

    SIG_GetMask(mask);

  /* count the number of masked signals */
    for (i = 0, n = 0;  i < NUM_SYSTEM_SIGS;  i++) {
	if (SIG_isSet(mask, SigInfo[i].id)) n++;
    }

    if (n == 0)
	return OPTION_NONE;
    else if (n == NUM_SYSTEM_SIGS)
	sigList = LIST_nil;
    else {
	for (i = 0, sigList = LIST_nil;  i < NUM_SYSTEM_SIGS;  i++) {
	    if (SIG_isSet(mask, SigInfo[i].id)) {
		name = ML_CString (msp, SigInfo[i].name);
		REC_ALLOC2(msp, sig, INT_CtoML(SigInfo[i].id), name);
		LIST_cons(msp, sigList, sig, sigList);
	    }
	}
    }

    OPTION_SOME(msp, res, sigList);
    return res;

} /* end of GetSignalMask */
