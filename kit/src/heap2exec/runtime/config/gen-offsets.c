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
    fprintf(f, "#define %sOffVSP %d\n", (sym), VOFFSET(fld))
#define PMOFFSET(sym, fld)	\
    fprintf(f, "#define %sOffMSP %d\n", (sym), MOFFSET(fld))

#if defined(TARGET_HPPA)
# define PROOT(sym, index)	\
       fprintf(f, "#define %sOffMSP RootsOffMSP+%d\n", (sym), WORD_SZB*index)
#else
# define PROOT(sym, index)	\
       fprintf(f, "#define %sOffMSP (RootsOffMSP+%d)\n", (sym), WORD_SZB*index)
#endif

main ()
{
    union {
	vproc_state_t	s;
	char		b[sizeof(vproc_state_t)];
    }		V;
    union {
	ml_state_t	s;
	char		b[sizeof(ml_state_t)];
    }		M;
    int		i;
    FILE	*f;

  /* check to make sure that the ml_roots array is 8-byte aligned */
    if ((MOFFSET(ml_roots[0]) & 7) != 0) {
	fprintf (stderr, "Error: ml_roots not 8-byte aligned\n");
	exit (1);
    }

    f = OpenFile ("mlstate-offsets.h", "_MLSTATE_OFFSETS_");

#if TARGET_BYTECODE
    fprintf (f, "/* TARGET_BYTECODE */\n");
#else
    PMOFFSET("VProc", ml_vproc);
    PMOFFSET("AllocPtr", ml_allocPtr);
    PMOFFSET("LimitPtr", ml_limitPtr);
    PMOFFSET("StorePtr", ml_storePtr);
    PMOFFSET("Roots", ml_roots[0]);
    PROOT("PC", PC_INDX);
    PROOT("StdArg", ARG_INDX);
    PROOT("StdCont", CONT_INDX);
    PROOT("StdClos", CLOSURE_INDX);
    PROOT("ExnPtr", EXN_INDX);
#ifdef BASE_INDX
    PROOT("BasePtr", BASE_INDX);
#endif
    PROOT("VarPtr", VAR_INDX);
    PROOT("LinkReg", LINK_INDX);
#ifdef MISC0_INDX
#   if defined(TARGET_HPPA)
       fprintf(f, "#define MiscRegOffMSP(i) %d+(%d*(i))\n",
	       ((Addr_t)&(M.s.ml_roots[MISC0_INDX])) - (Addr_t)&(M.b[0]),
	       WORD_SZB);
#   else
       fprintf(f, "#define MiscRegOffMSP(i) (%d+(%d*(i)))\n",
	       ((Addr_t)&(M.s.ml_roots[MISC0_INDX])) - (Addr_t)&(M.b[0]),
	       WORD_SZB);
#   endif
#endif
#ifdef N_PSEUDO_REGS
    for (i = 0;  i < N_PSEUDO_REGS;  i++) {
	char	buf[32];
	sprintf (buf, "PseudoReg%d", i+1);
	PMOFFSET(buf, ml_pseudoRegs[i]);
    }
#endif
#ifdef ICOUNT
    PMOFFSET("ICountRef", ml_icountReg);
#endif
    PMOFFSET("Mask", ml_liveRegMask);
#ifdef SOFT_POLL
    PMOFFSET("RealLimit", ml_realLimit);
    PMOFFSET("PollPending", ml_pollPending);
    PMOFFSET("InPollHandler", ml_inPollHandler);
#endif
    PVOFFSET("InML", vp_inMLFlag);
    PVOFFSET("HandlerPending", vp_handlerPending);
    PVOFFSET("InSigHandler", vp_inSigHandler);
    PVOFFSET("NPendingSys", vp_numPendingSysSigs);
    PVOFFSET("NPending", vp_numPendingSigs);
#endif /* !BYTECODE */

    CloseFile (f, "_MLSTATE_OFFSETS_");

    exit (0);
}
