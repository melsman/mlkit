/* ml-state.h
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 *
 * This is the C view of the state of a ML computation.
 */

#ifndef _ML_STATE_
#define _ML_STATE_

#ifndef _ML_BASE_
#include "ml-base.h"
#endif

#ifndef _ML_ROOTS_
#include "ml-roots.h"
#endif

#if (!defined(_CNTR_) && defined(ICOUNT))
#include "cntr.h"
#endif


/* This table maps the register numbers of the argument registers used by the
 * code generator to the proper indices of the root vector.  The table is
 * defined in kernel/ml-state.c.
 */
extern int   ArgRegMap[N_ARG_REGS];


/** The ML state vector **
 */
/* typedef struct ml_state ml_state_t; */  /* defined in ml-base.h */
struct ml_state {
				/* ML task info */
    heap_t	*ml_heap;	    /* The heap for this ML task */
#   define      ml_allocArena	  ml_heap->allocBase
#   define	ml_allocArenaSzB  ml_heap->allocSzB
    vproc_state_t *ml_vproc;	    /* the VProc that this is running on */

				/* ML registers */
    ml_val_t	*ml_allocPtr;
    ml_val_t	*ml_limitPtr;
/* NOTE: the ml_roots array must be 8-byte aligned */
    ml_val_t	ml_roots[NROOTS];   /* the root registers */
#   define      ml_linkReg	  ml_roots[LINK_INDX]
#   define	ml_pc		  ml_roots[PC_INDX]
#   define	ml_exnCont	  ml_roots[EXN_INDX]
#   define	ml_arg		  ml_roots[ARG_INDX]
#   define	ml_cont		  ml_roots[CONT_INDX]
#   define	ml_closure	  ml_roots[CLOSURE_INDX]
#   define      ml_varReg         ml_roots[VAR_INDX]
#ifdef BASE_INDX
#   define	ml_baseReg	  ml_roots[BASE_INDX]
#endif
#ifdef ICOUNT_INDX
#   define      ml_icount	  ml_roots[ICOUNT_INDX]
#endif
#    define	ml_misc0	  ml_roots[MISC0_INDX]
#    define	ml_misc1	  ml_roots[MISC1_INDX]
#    define	ml_misc2	  ml_roots[MISC2_INDX]
#    define	ml_misc3	  ml_roots[MISC3_INDX]
#    define	ml_misc4	  ml_roots[MISC4_INDX]
#    define	ml_misc5	  ml_roots[MISC5_INDX]
#    define	ml_misc6	  ml_roots[MISC6_INDX]
#    define	ml_misc7	  ml_roots[MISC7_INDX]
#if (CALLEESAVE > 0)
#    define	ml_calleeSave(i)  ml_roots[ArgRegMap[(i)+4]]
#endif

#ifdef N_PSEUDO_REGS
    ml_val_t	ml_pseudoRegs[N_PSEUDO_REGS];
#endif

    ml_val_t	ml_storePtr;	    /* the list of store operations */

				  /* Linkage information */
    Word_t	ml_liveRegMask;		/* The mask defining the live root registers */
    ml_val_t	ml_faultExn;		/* The exception packet for a hardware fault. */
    Word_t	ml_faultPC;		/* the PC of the faulting instruction */
#ifdef SOFT_POLL
    ml_val_t    *ml_realLimit;          /* real heap limit */
    bool_t      ml_pollPending;         /* poll event pending? */
    bool_t      ml_inPollHandler;       /* handling a poll event? */
#endif
#ifdef ICOUNT
    Word_t	ml_icountReg;		/* the contents of the instruction */
					/* counter register. */
    cntr_t	ml_icount;		/* The cumlative instruction count */
#endif
}; /* struct ml_state */


/* set up the return linkage and continuation throwing in the ML state vector. */
#if (CALLEESAVE > 0)
#define SETUP_RETURN(msp)	{					\
	ml_state_t	*__msp = (msp);					\
	__msp->ml_closure	= ML_unit;				\
	__msp->ml_pc		= __msp->ml_cont;			\
    }
#define SETUP_THROW(msp, cont, val)	{				\
	ml_state_t	*__msp = (msp);					\
	ml_val_t	__cont = (cont);				\
	__msp->ml_closure	= __cont;				\
	__msp->ml_cont		= ML_unit;				\
	__msp->ml_pc		=					\
	__msp->ml_linkReg	= GET_CODE_ADDR(__cont);		\
	__msp->ml_exnCont	= ML_unit;				\
	__msp->ml_arg		= (val);				\
    }

#else
#define SETUP_RETURN(msp)	{					\
	ml_state_t	*__msp = (msp);					\
	__msp->ml_closure	= __msp->ml_cont;			\
	__msp->ml_pc		=					\
	__msp->ml_linkReg	= GET_CODE_ADDR(__msp->ml_cont);	\
    }
#define SETUP_THROW(msp, cont, val)	{				\
	ml_state_t	*__msp = (msp);					\
	ml_val_t	__cont = (cont);				\
	__msp->ml_closure	= __cont;				\
	__msp->ml_cont		= ML_unit;				\
	__msp->ml_pc		=					\
	__msp->ml_linkReg	= GET_CODE_ADDR(__cont);		\
	__msp->ml_exnCont	= ML_unit;				\
	__msp->ml_arg		= (val);				\
    }
#endif

#endif /* !_ML_STATE_ */

