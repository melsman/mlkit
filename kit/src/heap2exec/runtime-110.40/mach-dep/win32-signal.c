/* win32-signal.c
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 *
 * when "signals" are supported in win32, they'll go here.
 */

#include "signal-sysdep.h"
#include "ml-base.h"
#include "ml-limits.h"
#include "ml-state.h"
#include "vproc-state.h"
#include "ml-objects.h"
#include "ml-signals.h"
#include "system-signals.h"
#include "ml-globals.h"

#include "win32-sigtbl.c"

#ifndef MP_SUPPORT
#define SELF_VPROC	(VProc[0])
#else
/** for MP_SUPPORT, we'll use SELF_VPROC for now **/
#define SELF_VPROC	(VProc[0])
#endif

/* ListSignals:
 */
ml_val_t ListSignals (ml_state_t *msp)
{
#ifdef WIN32_DEBUG
    SayDebug("win32:ListSignals: returning dummy signal list\n");
#endif
    return ML_SysConstList (msp, &SigTbl);
} 

/* PauseUntilSignal:
 *
 * Suspend the given VProc until a signal is received.
 */
void PauseUntilSignal (vproc_state_t *vsp)
{
#ifdef WIN32_DEBUG
    SayDebug("win32:PauseUntilSignal: returning without pause\n");
#endif
} 

/* SetSignalState:
 */
void SetSignalState (vproc_state_t *vsp, int sigNum, int sigState)
{
#ifdef WIN32_DEBUG
    SayDebug("win32:SetSignalState: not setting state for signal %d\n",sigNum);
#endif
}


/* GetSignalState:
 */
int GetSignalState (vproc_state_t *vsp, int sigNum)
{
#ifdef WIN32_DEBUG
    SayDebug("win32:GetSignalState: returning state for signal %d as ML_SIG_DEFAULT\n",sigNum);
#endif
    return ML_SIG_DEFAULT;
}  


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
#ifdef WIN32_DEBUG
    SayDebug("win32:SetSigMask: not setting mask\n");
#endif
}


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
#ifdef WIN32_DEBUG
    SayDebug("win32:GetSignalMask: returning mask as NONE\n");
#endif
    return OPTION_NONE;
}

/* end of win32-signal.c */
