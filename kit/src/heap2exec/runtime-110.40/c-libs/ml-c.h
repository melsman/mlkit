/* ml-c.h
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * Header file for C functions that are callable from ML.  This defines
 * a number of macros for checking return results and for raising the SysErr
 * exception:
 *
 *	RAISE_SYSERR(msp, sts)		Raise the SysErr exception using the
 *					appropriate system error message (on
 *					some systems, sts may be an error code).
 *
 *	RAISE_ERROR(msp, msg)		Raise the SysErr exception using the
 *					given message (with NONE for the system
 *					error part).
 *
 *	CHK_RETURN_VAL(msp, sts, val)	Check sts for an error (< 0); if okay,
 *					then return val.  Otherwise raise
 *					SysErr with the appropriate system
 *					error message.
 *
 *	CHK_RETURN(msp, sts)		Check sts for an error (< 0); if okay,
 *					then return it as the result (after
 *					converting to an ML int).
 *
 *	CHK_RETURN_UNIT(msp, sts)	Check sts for an error (< 0); if okay,
 *					then return unit.
 */

#ifndef _ML_C_
#define _ML_C_

#ifndef _ML_OSDEP_
#include "ml-osdep.h"
#endif


#ifdef SYSCALL_RET_ERR
ml_val_t RaiseSysError (ml_state_t *msp, int err, const char *alt_msg, const char *at);
#define RAISE_SYSERR(msp, sts)	\
	RaiseSysError((msp), (sts), NIL(char *), "<" __FILE__ ">")
#define RAISE_ERROR(msp, msg)	\
	RaiseSysError((msp), 0, (msg), "<" __FILE__ ">")

#else
ml_val_t RaiseSysError (ml_state_t *msp, const char *alt_msg, const char *at);
#define RAISE_SYSERR(msp, sts)	\
	RaiseSysError((msp), NIL(char *), "<" __FILE__ ">")
#define RAISE_ERROR(msp, msg)	\
	RaiseSysError((msp), (msg), "<" __FILE__ ">")

#endif

/* return a value to the calling ML code, but raise an exception if an error
 * occured.
 */
#define CHK_RETURN_VAL(msp,sts,val)	{			\
	if ((sts) < 0)						\
	    return RAISE_SYSERR(msp, sts);			\
	else							\
	    return (val);					\
    }

/* return sts to the calling ML code, but raise an exception if an error occured */
#define CHK_RETURN(msp,sts)	{				\
	int	__sts = (sts);					\
	CHK_RETURN_VAL((msp), __sts, INT_CtoML(__sts))		\
    }

/* return unit to the calling ML code, but raise an exception if an error occured */
#define CHK_RETURN_UNIT(msp,sts)				\
	CHK_RETURN_VAL(msp, sts, ML_unit)

#endif /* !_ML_C_ */
