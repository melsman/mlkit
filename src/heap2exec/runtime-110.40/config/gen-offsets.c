/* gen-offsets.c
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 *
 * This C program generates a header file for the *.prim.asm files,
 * which gives the offset values in the VProc and ML state vectors.
 */

#include "ml-base.h"
#include "vproc-state.h"
#include "ml-state.h"
#include "gen.h"

#define MOFFSET(fld)	(((Addr_t)&(M.s.fld)) - (Addr_t)&(M.b[0]))
#define VOFFSET(fld)	(((Addr_t)&(V.s.fld)) - (Addr_t)&(V.b[0]))

#define PVOFFSET(sym, fld)	\
    fprintf(f, "#define %sOffVSP %ld\n", (sym), (long int) VOFFSET(fld))
#define PMOFFSET(sym, fld)	\
    fprintf(f, "#define %sOffMSP %ld\n", (sym), (long int) MOFFSET(fld))


int main (void)
{
    union {
	vproc_state_t	s;
	char		b[sizeof(vproc_state_t)];
    }		V;
    union {
	ml_state_t	s;
	char		b[sizeof(ml_state_t)];
    }		M;
    FILE	*f;

    f = OpenFile ("mlstate-offsets.h", "_MLSTATE_OFFSETS_");

#if TARGET_BYTECODE
    fprintf (f, "/* TARGET_BYTECODE */\n");
#else
    PMOFFSET("VProc", ml_vproc);
    PMOFFSET("AllocPtr", ml_allocPtr);
    PMOFFSET("LimitPtr", ml_limitPtr);
    PMOFFSET("StorePtr", ml_storePtr);
    PMOFFSET("StdArg", ml_arg);
    PMOFFSET("StdCont", ml_cont);
    PMOFFSET("StdClos", ml_closure);
    PMOFFSET("LinkReg", ml_linkReg);
    PMOFFSET("PC", ml_pc);
    PMOFFSET("ExnPtr", ml_exnCont);
    PMOFFSET("VarPtr", ml_varReg);
    PMOFFSET("Misc0", ml_calleeSave[0]);
    PMOFFSET("Misc1", ml_calleeSave[1]);
    PMOFFSET("Misc2", ml_calleeSave[2]);
#ifdef SOFT_POLL
    PMOFFSET("RealLimit", ml_realLimit);
    PMOFFSET("PollPending", ml_pollPending);
    PMOFFSET("InPollHandler", ml_inPollHandler);
#endif
    PVOFFSET("InML", vp_inMLFlag);
    PVOFFSET("LimitPtrMask", vp_limitPtrMask);
    PVOFFSET("HandlerPending", vp_handlerPending);
    PVOFFSET("InSigHandler", vp_inSigHandler);
    PVOFFSET("NPendingSys", vp_numPendingSysSigs);
    PVOFFSET("NPending", vp_numPendingSigs);
#endif /* !BYTECODE */

    CloseFile (f, "_MLSTATE_OFFSETS_");

    exit (0);
}
