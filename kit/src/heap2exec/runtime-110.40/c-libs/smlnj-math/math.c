/* math.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "tags.h"
#include "ml-objects.h"
#include "ml-fp.h"

#include <math.h>

#define REAL_ALLOC(msp, r, d)	{			\
	ml_state_t	*__msp = (msp);			\
	ml_val_t        *__p = __msp->ml_allocPtr;	\
	double          *__dp;                          \
	*__p++ = DESC_reald;				\
	__dp = (double *) __p;                          \
	*__dp++ = (d);          			\
	(r) = PTR_CtoML(__msp->ml_allocPtr + 1);	\
	__msp->ml_allocPtr = (ml_val_t *) __dp;		\
    }

ml_val_t c_cos(ml_state_t *msp, ml_val_t arg)
{
    double d;
    ml_val_t res;

    Save_ML_FPState();
    Restore_C_FPState();
    d = cos(*(PTR_MLtoC(double,arg)));
    REAL_ALLOC(msp,res,d);
    Restore_ML_FPState();
    return res;
}

ml_val_t c_sin(ml_state_t *msp, ml_val_t arg)
{
    double d;
    ml_val_t res;

    Save_ML_FPState();
    Restore_C_FPState();
    d = sin(*(PTR_MLtoC(double,arg)));
    REAL_ALLOC(msp,res,d);
    Restore_ML_FPState();
    return res;
}

ml_val_t c_exp(ml_state_t *msp, ml_val_t arg)
{
    double d;
    ml_val_t res;
    extern int errno;

    Save_ML_FPState();
    Restore_C_FPState();
    errno = 0;
    d = exp(*(PTR_MLtoC(double,arg)));
    REAL_ALLOC(msp,res,d);
    REC_ALLOC2(msp,res,res,INT_CtoML(errno));
    Restore_ML_FPState();
    return res;
}

ml_val_t c_log(ml_state_t *msp, ml_val_t arg)
{
    double d;
    ml_val_t res;

    Save_ML_FPState();
    Restore_C_FPState();
    d = log(*(PTR_MLtoC(double,arg)));
    REAL_ALLOC(msp,res,d);
    Restore_ML_FPState();
    return res;
}

ml_val_t c_atan(ml_state_t *msp, ml_val_t arg)
{
    double d;
    ml_val_t res;

    Save_ML_FPState();
    Restore_C_FPState();
    d = atan(*(PTR_MLtoC(double,arg)));
    REAL_ALLOC(msp,res,d);
    Restore_ML_FPState();
    return res;
}

ml_val_t c_sqrt(ml_state_t *msp, ml_val_t arg)
{
    double d;
    ml_val_t res;

    Save_ML_FPState();
    Restore_C_FPState();
    d = sqrt(*(PTR_MLtoC(double,arg)));
    REAL_ALLOC(msp,res,d);
    Restore_ML_FPState();
    return res;
}

/* end of math.c */

