/* signal-sysdep.h
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * O.S. and machine dependent signal definitions for UNIX systems:
 *
 *   typedef SigReturn_t        the return type of a signal handler.
 *   typedef SigInfo_t          the signal generation information passed to a
 *                              a signal handler.
 *   typedef SigContext_t       the context info passed to a signal handler.
 *   typedef SigMask_t		the representation of a set of signals
 *
 *   SIG_GetCode(info, scp)	extract the signal generation information
 *   SIG_GetPC(scp)		get the PC from the context
 *   SIG_SetPC(scp, addr)	set the PC in the context to the address
 *   SIG_SetHandler(sig, h)	set the signal handler
 *   SIG_GetHandler(sig, h)	get the current handler into h
 *   SIG_Flags			flags used when setting a handler
 *   SIG_ClearMask(mask)	clear the given signal mask.
 *   SIG_AddToMask(mask, sig)	Add the given signal to the mask.
 *   SIG_isSet(mask, sig)	Return true, if the signal is in the mask.
 *   SIG_SetMask(mask)		Set the signal mask.
 *   SIG_GetMask(mask)		Get the signal mask into the variable mask.
 *
 *   SIG_FAULT[12]		The signals used to detect faults.
 *
 *   SIG_InitFPE()		This macro is defined to be a routine for
 *				initializing the FPE hardware exception mechanism.
 *
 *   SIG_ResetFPE(scp)		This macro is defined to be a routine for resetting
 *				the signal handling state (or hardware status
 *				registers) on machines that require it; otherwise
 *				it is defined to the empty statement.
 *
 * Predicates on signals, the arguments are (signal, code).
 *   INT_DIVZERO(s, c)
 *   INT_OVFLW(s, c)
 *
 * There are two ways to force a GC when a signal occurs.  For some machines,
 * this is done in an assembly routine called ZeroLimitPtr; for others, this
 * can be done directly by manipulating the signal context.  The following
 * macros are used for this purpose:
 *
 *   USE_ZERO_LIMIT_PTR_FN	If set, then we use the ZeroLimitPtr function.
 *   SIG_SavePC(msp, scp)	Save the PC, so that ZeroLimitPtr can restore it.
 *
 *   SIG_ZeroLimitPtr(scp)	Set the limit pointer in the context to zero.
 *
 * NOTE: Currently SavedPC is a global (so that the asm code in adjust_limit
 * can access it).  Once we have a runtimeLink register that allows dynamic
 * access to the MLState, we can move SavedPC to the ML State vector.
 */

#ifndef _SIGNAL_SYSDEP_
#define _SIGNAL_SYSDEP_

#ifndef _ML_OSDEP_
#include "ml-osdep.h"
#endif

#ifndef _ML_BASE_
#include "ml-base.h"	/* for Addr_t */
#endif

#if defined(OPSYS_UNIX)
#  include <signal.h>
#endif

#if defined(HAS_UCONTEXT)
#include <ucontext.h>
#include <siginfo.h>

typedef void SigReturn_t;
typedef siginfo_t *SigInfo_t;
typedef ucontext_t SigContext_t;

#define SIG_FAULT1	SIGFPE

#define INT_DIVZERO(s, c)	(((s) == SIGFPE) && ((c) == FPE_INTDIV))
#define INT_OVFLW(s, c)		(((s) == SIGFPE) && ((c) == FPE_INTOVF))

#define SIG_GetCode(info,scp)	((info)->si_code)
#define SIG_Flags		SA_SIGINFO

#elif defined(HAS_SIGCONTEXT)

typedef int SigInfo_t;
typedef struct sigcontext SigContext_t;
#  define SIG_Flags		0
#endif


#if defined(HAS_POSIX_SIGS)
/** POSIX signals **/
#define SIG_SetHandler(sig, h)	{       		\
	struct sigaction __svec;        		\
	sigfillset(&(__svec.sa_mask));  		\
	__svec.sa_flags = SIG_Flags;			\
	__svec.sa_handler = (h);        		\
	sigaction ((sig), &__svec, 0);  		\
    }
#define SIG_GetHandler(sig, h)  {				\
	struct sigaction __svec;				\
	sigaction ((sig), NIL(struct sigaction *), &__svec);	\
	(h) = __svec.sa_handler;				\
    }
typedef sigset_t SigMask_t;
#define SIG_ClearMask(mask) 	sigemptyset(&(mask))
#define SIG_AddToMask(mask, s)	sigaddset(&(mask), (s))
#define SIG_isSet(mask, s)	sigismember(&(mask), (s))
#define SIG_SetMask(mask)	sigprocmask(SIG_SETMASK, &(mask), NIL(sigset_t *))
#define SIG_GetMask(mask)	sigprocmask(SIG_SETMASK, NIL(sigset_t *), &(mask))

#elif defined(HAS_BSD_SIGS)
/** BSD signals **/
#define SIG_SetHandler(sig, h)	{       		\
	struct sigvec __svec;               		\
	__svec.sv_mask = 0xFFFFFFFF;        		\
	__svec.sv_flags = SV_INTERRUPT;			\
	__svec.sv_handler = (h);            		\
	sigvec ((sig), &__svec, 0);         		\
    }
#define SIG_GetHandler(sig, h)  {			\
	struct sigvec __svec;				\
	sigvec ((sig), NIL(struct sigvec *), &__svec);	\
	(h) = __svec.sv_handler;			\
    }
typedef int SigMask_t;
#define SIG_ClearMask(mask)	((mask) = 0)
#define SIG_AddToMask(mask, s)	((mask) |= sigmask(s))
#define SIG_isSet(mask, s)	(((mask) & sigmask(s)) != 0)
#define SIG_SetMask(mask)	sigsetmask(mask)
#define SIG_GetMask(mask)	{			\
	int		__tmpMask;			\
	__tmpMask = 0xFFFFFFFF;				\
	(mask) = sigsetmask(__tmpMask);			\
	sigsetmask(mask);				\
    }
#elif defined(OPSYS_WIN32)
  /* no win32 signals yet */
#else
#  error no way to set signal handler
#endif


/** Machine/OS dependent stuff **/

#if defined(HOST_SPARC)

extern void SetFSR(int);
  /* disable all FP exceptions */
#  define SIG_InitFPE()    SetFSR(0)

#  if defined(OPSYS_SUNOS)
    /** SPARC, SUNOS **/
#    define USE_ZERO_LIMIT_PTR_FN
#    define SIG_FAULT1		SIGFPE
#    define INT_DIVZERO(s, c)	(((s) == SIGFPE) && ((c) == FPE_INTDIV_TRAP))
#    define INT_OVFLW(s, c)	(((s) == SIGFPE) && ((c) == FPE_INTOVF_TRAP))
#    define SIG_GetCode(info, scp)	(info)
#    define SIG_GetPC(scp)	((scp)->sc_pc)
#    define SIG_SetPC(scp, addr)	{			\
	(scp)->sc_pc = (long)(addr);				\
	(scp)->sc_npc = (scp)->sc_pc + 4;			\
    }
#    define SIG_SavePC(msp, scp)	{			\
	SigContext_t	*__scp = (scp);				\
	long		__pc = __scp->sc_pc;			\
	if (__pc+4 != __scp->sc_npc)				\
	  /* the pc is pointing to a delay slot, so back-up	\
	   * to the branch. */					\
	    __pc -= 4;						\
	SavedPC = __pc;						\
    }
     typedef void SigReturn_t;

#  elif defined(OPSYS_SOLARIS)
    /** SPARC, SOLARIS **/
#    define SIG_GetPC(scp)		((scp)->uc_mcontext.gregs[REG_PC])
#    define SIG_SetPC(scp, addr)	{			\
	(scp)->uc_mcontext.gregs[REG_PC] = (long)(addr);	\
	(scp)->uc_mcontext.gregs[REG_nPC] = (long)(addr) + 4;	\
    }
#    define SIG_ZeroLimitPtr(scp)	\
	{ (scp)->uc_mcontext.gregs[REG_G4] = 0; }

#  endif

#elif defined(HOST_MIPS)

extern void SetFSR();
#  define SIG_InitFPE()    SetFSR()

#  if defined(OPSYS_IRIX4)
    /** MIPS, IRIX 4.0.x **/
#    define SIG_FAULT1		SIGFPE
#    define SIG_FAULT2		SIGTRAP
#    include <sys/sbd.h>  /* for EXC_OV */
#    define INT_DIVZERO(s, c)	(((s) == SIGTRAP) && ((c) == BRK_DIVZERO))
#    define INT_OVFLW(s, c)	(((s) == SIGTRAP) && ((c) == BRK_OVERFLOW))
#    define SIG_GetCode(info, scp)	((info) ? (info) : (scp)->sc_fpc_csr)
#    define SIG_GetPC(scp)		((scp)->sc_pc)
#    define SIG_SetPC(scp, addr)	{ (scp)->sc_pc = (long)(addr); }
#    define SIG_ZeroLimitPtr(scp)	{ (scp)->sc_regs[19] = 0; }
     typedef void SigReturn_t;

#  elif defined(OPSYS_IRIX5)
    /** MIPS, IRIX 5.x **/
#    define SIG_FAULT2		SIGTRAP
   /* We use a TRAP to signal zero divide on the mips, but IRIX 5.3 maps
    * this back to SIGFPE.
    */
#    undef INT_DIVZERO		/* SIGTRAP used for this on MIPS */
#    define INT_DIVZERO(s, c)	\
	(((s) == SIGTRAP) || (((s) == SIGFPE) && ((c) == FPE_INTDIV)))

#    define SIG_GetPC(scp)		((scp)->uc_mcontext.gregs[CTX_EPC])
#    define SIG_SetPC(scp, addr)	\
	{ (scp)->uc_mcontext.gregs[CTX_EPC] = (long)(addr); }
#    define SIG_ZeroLimitPtr(scp)	\
	{ (scp)->uc_mcontext.gregs[CTX_S3] = 0; }
#  endif /* ARCH_MIPS */

#elif defined(HOST_RS6000)
#  if defined (OPSYS_AIX)
    /** RS6000, AIX **/
#    include <fpxcp.h>
#    define SIG_FAULT1		SIGTRAP

#    define INT_DIVZERO(s, c)	(((s) == SIGTRAP) && ((c) & FP_DIV_BY_ZERO))
#    define INT_OVFLW(s, c)	(((s) == SIGTRAP) && ((c) == 0))
     PVT int SIG_GetCode (SigInfo_t info, SigContext_t *scp);
#    define SIG_GetPC(scp)	((scp)->sc_jmpbuf.jmp_context.iar)
#    define SIG_SetPC(scp, addr)	\
	{ (scp)->sc_jmpbuf.jmp_context.iar = (long)(addr); }
#    define SIG_ZeroLimitPtr(scp)	\
	{ (scp)->sc_jmpbuf.jmp_context.gpr[15] = 0; }
#    define SIG_ResetFPE(scp)	{						\
	    SigContext_t	*__scp = (scp);					\
	    struct mstsave	*__scj = &(__scp->sc_jmpbuf.jmp_context);	\
	    fp_ctx_t		__flt_ctx;					\
	    __scj->xer &= 0x3fffffff;						\
	    fp_sh_trap_info (__scp, &__flt_ctx);				\
	    fp_sh_set_stat (__scp, (__flt_ctx.fpscr & ~__flt_ctx.trap));	\
	}
     typedef void SigReturn_t;

#  elif defined(OPSYS_MKLINUX)
    /* RS6000, MkLinux */

#    include "mklinux-regs.h"
     typedef struct mklinux_ppc_regs SigContext_t;

#    define SIG_FAULT1		SIGILL

#    define INT_DIVZERO(s, c)		(((s) == SIGILL) && ((c) == 0x84000000))
#    define INT_OVFLW(s, c)		(((s) == SIGILL) && ((c) == 0x0))
#    define SIG_GetPC(scp)		((scp)->nip)
#    define SIG_SetPC(scp, addr)	{ (scp)->nip = (long)(addr); }
#    define SIG_ZeroLimitPtr(scp)	{ ((scp)->gpr[15] = 0); }
#    define SIG_GetCode(info,scp)	((scp)->fpscr)
#    define SIG_ResetFPE(scp)		{ (scp)->fpscr = 0x0; }
     typedef void SigReturn_t;

#    define SIG_Flags		0

#  endif /* HOST_RS6000 */

#elif defined(HOST_HPPA)

#  if defined(OPSYS_HPUX9)
    /** HPPA, HPUX 9.x **/
     typedef void SigReturn_t;
#    define SIG_FAULT1 SIGFPE
    /* Since exceptions can be raised both in data space and code space,
     * implementing this on HPPA/HPUX is going to be complicated.
     */
#    define SIG_GetPC(scp)	0
    /* pcoq and pcsq are equivalent to the instruction address
     * offset queue (iaoq) and the IA space queue (iasq)
     */
#    define SIG_SetPC(scp, addr) {					\
	SigContext_t *_scp = (scp);					\
	_scp->sc_pcoq_head = addr;					\
	_scp->sc_pcoq_tail = _scp->sc_pcoq_head + 4;			\
	_scp->sc_pcsq_tail = _scp->sc_pcsq_head = pointer2space(addr);	\
    }
#    define SIG_ZeroLimitPtr(scp)	{ (scp)->sc_gr4 = 0; }

#    define SIG_GetCode(info, scp)	info
	 
#    define INT_DIVZERO(s, c)  (((s) == SIGFPE) && ((c) == 13)) 
#    define INT_OVFLW(s, c)    (((s) == SIGFPE) && ((c) == 12 || (c) == 14))
#    define SIG_InitFPE()      set_fsr()

#  endif

#  if defined(OPSYS_HPUX)
    /** HPPA, HPUX 10.x **/

#    define SIG_FAULT1 SIGFPE

    /* There are bugs in the HPUX *.10.* machine/save_state.h
     * header file macros!! 
     */
#    define sc_pcoq_head sc_sl.sl_ss.ss_narrow.ss_pcoq_head
#    define sc_pcoq_tail sc_sl.sl_ss.ss_narrow.ss_pcoq_tail
#    define sc_pcsq_head sc_sl.sl_ss.ss_narrow.ss_pcsq_head
#    define sc_pcsq_tail sc_sl.sl_ss.ss_narrow.ss_pcsq_tail
#    define sc_gr3 sc_sl.sl_ss.ss_narrow.ss_gr3
#    define sc_gr4 sc_sl.sl_ss.ss_narrow.ss_gr4

    /* Since exceptions can be raised both in data space and code space,
     * implementing this on HPPA/HPUX is going to be complicated.
     */
#    define SIG_GetPC(scp)	0
    /*	pcoq and pcsq are equivalent to the instruction address
     * offset queue (iaoq) and the IA space queue (iasq)
     */
#    define SIG_SetPC(scp, addr) {					\
	SigContext_t *_scp = (scp);					\
	_scp->sc_pcoq_head = addr;					\
	_scp->sc_pcoq_tail = _scp->sc_pcoq_head + 4;			\
	_scp->sc_pcsq_tail = _scp->sc_pcsq_head = pointer2space(addr);	\
    }

#    define SIG_ZeroLimitPtr(scp)	{ (scp)->sc_gr4 = 0; }
#    define SIG_GetCode(info, scp)	(info)

    /* The SVR4 API for SIGFPE isn't implemented correctly */
#    undef INT_DIVZERO
#    undef INT_OVFLW
#    define INT_DIVZERO(s, c)  (((s) == SIGFPE) && ((c) == 0xd)) 
#    define INT_OVFLW(s, c)    (((s) == SIGFPE) && ((c) == 0xc || (c) == 0xe))

#    define SIG_InitFPE()      set_fsr()

     typedef void SigReturn_t;

#  endif

#elif defined(HOST_X86)

#  define LIMITPTR_X86OFFSET	3	/* offset (words) of limitptr in ML stack */
					/* frame (see X86.prim.asm) */
extern Addr_t *ML_X86Frame;   /* used to get at limitptr */
#  define SIG_InitFPE()    FPEEnable()

#  if defined(OPSYS_LINUX)
    /** X86, LINUX **/
#    if (!defined(_SIGCONTEXT_H) && !defined(sigcontext_struct))
      /* older versions of Linux don't define this in <signal.h> */
	struct sigcontext {
	    unsigned short gs, __gsh;
	    unsigned short fs, __fsh;
	    unsigned short es, __esh;
	    unsigned short ds, __dsh;
	    unsigned long edi;
	    unsigned long esi;
	    unsigned long ebp;
	    unsigned long esp;
	    unsigned long ebx;
	    unsigned long edx;
	    unsigned long ecx;
	    unsigned long eax;
	    unsigned long trapno;
	    unsigned long err;
	    unsigned long eip;
	    unsigned short cs, __csh;
	    unsigned long eflags;
	    unsigned long esp_at_signal;
	    unsigned short ss, __ssh;
	    unsigned long i387;
	    unsigned long oldmask;
	    unsigned long cr2;
	};
#    endif

#define INTO_OPCODE		0xce	/* the 'into' instruction is a single */
					/* instruction that signals Overflow */


#    define SIG_FAULT1		SIGFPE
#    define SIG_FAULT2		SIGSEGV
#    define INT_DIVZERO(s, c)	((s) == SIGFPE)
#    define INT_OVFLW(s, c)	\
	(((s) == SIGSEGV) && (((Byte_t *)c)[-1] == INTO_OPCODE))

#    define SIG_GetCode(info,scp)	((scp)->eip)
/* for linux, SIG_GetCode simply returns the address of the fault */
#    define SIG_GetPC(scp)		((scp)->eip)
#    define SIG_SetPC(scp,addr)		{ (scp)->eip = (long)(addr); }
#    define SIG_ZeroLimitPtr(scp)	{ ML_X86Frame[LIMITPTR_X86OFFSET] = 0; }
     typedef void SigReturn_t;

#  elif defined(OPSYS_FREEBSD)
    /** x86, FreeBSD **/
#    define SIG_FAULT1		SIGFPE
#    define INT_DIVZERO(s, c)	(((s) == SIGFPE) && ((c) == FPE_INTDIV_TRAP))
#    define INT_OVFLW(s, c)	(((s) == SIGFPE) && ((c) == FPE_INTOVF_TRAP))

#    define SIG_GetCode(info, scp)	(info)
#    define SIG_GetPC(scp)		((scp)->sc_pc)
#    define SIG_SetPC(scp, addr)	{ (scp)->sc_pc = (long)(addr); }
#    define SIG_ZeroLimitPtr(scp)	{ ML_X86Frame[LIMITPTR_X86OFFSET] = 0; }

     typedef void SigReturn_t;

#  elif defined(OPSYS_NETBSD)
    /** x86, NetBSD **/
/* NetBSD (including versions 1.0 and 1.1) generates SIGBUS rather
   than SIGFPE for overflows.  The real fix is a trivial change to
   kernel sources, which has already been reported (NetBSD internal
   problem identification "port-i386/1833"). 

   If you want to fix this on your NetBSD system.  Edit machdep.c in
   directory /sys/arch/i386/i386, and find the line

        setgate(&idt[  4], &IDTVEC(ofl),     0, SDT_SYS386TGT, SEL_KPL);

   Change SEL_KPL to SEL_UPL.  With SEL_KPL, the int overflow trap is
   not accessible at user level, and a protection fault occurs instead
   (thus the seg fault).  SEL_UPL will allow user processes to generate
   this trap.

   For the change to take effect, recompile your kernel, install it
   and reboot. */
#    define SIG_FAULT1		SIGFPE
#    define SIG_FAULT2		SIGBUS
#    define INT_DIVZERO(s, c)	0
#    define INT_OVFLW(s, c)	(((s) == SIGFPE) || ((s) == SIGBUS))

#    define SIG_GetCode(info, scp)	(info)
#    define SIG_GetPC(scp)		((scp)->sc_pc)
#    define SIG_SetPC(scp, addr)	{ (scp)->sc_pc = (long)(addr); }
#    define SIG_ZeroLimitPtr(scp)	{ ML_X86Frame[LIMITPTR_X86OFFSET] = 0; }

     typedef void SigReturn_t;

#  elif defined(OPSYS_SOLARIS)
     /** x86, Solaris */

#    define SIG_GetPC(scp)		((scp)->uc_mcontext.gregs[EIP])
#    define SIG_SetPC(scp, addr)	{ (scp)->uc_mcontext.gregs[EIP] = (int)(addr); }
#    define SIG_ZeroLimitPtr(scp)	{ ML_X86Frame[LIMITPTR_X86OFFSET] = 0; }

#  elif defined(OPSYS_WIN32)
#    define SIG_ZeroLimitPtr()		{ ML_X86Frame[LIMITPTR_X86OFFSET] = 0; }

#  else
#    error "unknown OPSYS for x86"
#  endif

#elif defined(HOST_ALPHA32)

#  if (defined(OPSYS_OSF1) || defined(OPSYS_DUNIX))
    /** Alpha AXP, OSF1 **/
#    include <machine/fpu.h>
#    define SIG_FAULT1		SIGFPE
#    define INT_DIVZERO(s, c)	(((s) == SIGFPE) && ((c) == -2))
#    define INT_OVFLW(s, c)	(((s) == SIGFPE) && ((c) == FPE_INTOVF_FAULT))
#    define SIG_GetPC(scp)		((scp)->sc_pc)
#    define SIG_SetPC(scp, addr)	{ (scp)->sc_pc = (long)(addr); }
#    define SIG_GetCode(info, scp)	info
#    define SIG_ZeroLimitPtr(scp)	{ (scp)->sc_regs[9] = 0; }
     typedef void SigReturn_t;
#    define SIG_InitFPE()	SetFSR()
#  endif
#endif

#ifndef SIG_InitFPE
#define SIG_InitFPE()		/* nop */
#endif

#ifndef SIG_ResetFPE
#define SIG_ResetFPE(SCP)	/* nop */
#endif

#endif /* !_SIGNAL_SYSDEP_ */

