/* unix-fault.c
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 *
 * Common code for handling arithmetic traps.
 */

#include "ml-unixdep.h"
#include "signal-sysdep.h"
#include "ml-base.h"
#include "vproc-state.h"
#include "ml-state.h"
#include "ml-globals.h"

/* this is temporary */
#define SELF_VPROC	(VProc[0])


/* local routines */
PVT SigReturn_t FaultHandler (/* int sig, SigInfo_t code, SigContext_t *scp */);
PVT SigReturn_t PanicTrace (/* int sig, SigInfo_t code, SigContext_t *scp */);


/* InitFaultHandlers:
 */
void InitFaultHandlers (ml_state_t *msp)
{
    int		mlSig;

#ifndef TARGET_BYTECODE

  /** Set up the Div and Overflow faults **/
#ifdef SIG_FAULT1
    SIG_SetHandler (SIG_FAULT1, FaultHandler);
#endif
#ifdef SIG_FAULT2
    SIG_SetHandler (SIG_FAULT2, FaultHandler);
#endif

  /** Initialize the floating-point unit **/
    SIG_InitFPE ();

#else /* TARGET_BYTECODE */
/** **/ SIG_SetHandler (SIGINT, PanicTrace);
        SIG_SetHandler (SIGBUS, PanicTrace);
        SIG_SetHandler (SIGSEGV, PanicTrace);
#endif /* !TARGET_BYTECODE */

} /* end of InitFaultHandlers */


#ifndef TARGET_BYTECODE

/* FaultHandler:
 *
 * Handle arithmetic faults (e.g., divide by zero, integer overflow).
 */
PVT SigReturn_t FaultHandler (
    int		    signal,
#if defined(OPSYS_LINUX)
    SigContext_t    sc)
#elif defined(OPSYS_MKLINUX)
    SigContext_t    *scp)
#else
    SigInfo_t	    info,
    SigContext_t    *scp)
#endif
{
#if defined(OPSYS_LINUX)
    SigContext_t    *scp = &sc;
#endif
    ml_state_t	    *msp = SELF_VPROC->vp_state;
    extern Word_t   request_fault[];
    int		    code = SIG_GetCode(info, scp);

#ifdef SIGNAL_DEBUG
    SayDebug ("Fault handler: sig = %d, inML = %d\n",
	signal, SELF_VPROC->vp_inMLFlag);
#endif

    if (! SELF_VPROC->vp_inMLFlag) 
	Die ("bogus fault not in ML: (%d, %#x)\n", signal, SIG_GetCode(info, scp));

   /* Map the signal to the appropriate ML exception. */
    if (INT_DIVZERO(signal, code)) {
	msp->ml_faultExn = DivId;
	msp->ml_faultPC = (Word_t)SIG_GetPC(scp);
    }
    else if (INT_OVFLW(signal, code)) {
	msp->ml_faultExn = OverflowId;
	msp->ml_faultPC = (Word_t)SIG_GetPC(scp);
    }
    else
	Die ("unexpected fault, signal = %d, code = %#x", signal, code);

    SIG_SetPC (scp, request_fault);

    SIG_ResetFPE (scp);

} /* end of FaultHandler */

#endif /* !TARGET_BYTECODE */


#if defined(TARGET_BYTECODE)
extern void PrintRegs (FILE *);
#ifdef INSTR_HISTORY
extern void PrintInstrHistory (FILE *);
#endif

/* PanicTrace:
 * This signal handler prints a trace of the last few instructions executed
 * by the bytecode interpreter (for debugging purposes).
 */
PVT SigReturn_t PanicTrace (
    int		    signal,
#if defined(OPSYS_LINUX)
    SigContext_t    sc)
#else
    SigInfo_t	    info,
    SigContext_t    *scp)
#endif
{
#if defined(OPSYS_LINUX)
    SigContext_t    *scp = &sc;
#endif

    SayDebug ("**** PANIC: signal = %d, code = %#x ****\n",
	signal, SIG_GetCode(info, scp));
    PrintRegs(DebugF);
#ifdef INSTR_HISTORY
    PrintInstrHistory (DebugF);
#endif
    SayDebug ("\n");

    Exit (1);
}
#endif


#if (defined(TARGET_RS6000) && defined(OPSYS_AIX))

/* SIG_GetCode:
 *
 * For the RS6000/AIX, the overflow and divide by zero information is obtained
 * from information contained in the sigcontext structure.
 */
PVT int SIG_GetCode (SigInfo_t code, SigContext_t *scp)
{
    struct fp_sh_info	FPInfo;

    fp_sh_info (scp, &FPInfo, sizeof(struct fp_sh_info));

    return FPInfo.trap;

} /* end of SIG_GetCode */

#endif
