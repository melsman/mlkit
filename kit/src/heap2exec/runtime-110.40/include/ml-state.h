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

#define CALLEESAVE	3


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
    ml_val_t	ml_arg;
    ml_val_t	ml_cont;
    ml_val_t	ml_closure;
    ml_val_t	ml_linkReg;
    ml_val_t	ml_pc;		    /* Address of ML code to execute; when */
				    /* calling an ML frunction from C, this */
				    /* holds the same value as the linkReg. */
    ml_val_t	ml_exnCont;
    ml_val_t	ml_varReg;
    ml_val_t	ml_calleeSave[CALLEESAVE];

    ml_val_t	ml_storePtr;	    /* the list of store operations */

				  /* Linkage information */
    ml_val_t	ml_faultExn;		/* The exception packet for a hardware fault. */
    Word_t	ml_faultPC;		/* the PC of the faulting instruction */
#ifdef SOFT_POLL
    ml_val_t    *ml_realLimit;          /* real heap limit */
    bool_t      ml_pollPending;         /* poll event pending? */
    bool_t      ml_inPollHandler;       /* handling a poll event? */
#endif
}; /* struct ml_state */


/* set up the return linkage and continuation throwing in the ML state vector. */
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

#endif /* !_ML_STATE_ */

