/* ctlrndmode.c
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 */

#include "ml-base.h"
#include "fp-dep.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"
#include "ml-c.h"

#ifndef NO_ROUNDING_MODE_CTL
/* Mapping between the ML and C representations of rounding modes. */
#if defined(RMODE_C_EQ_ML)
#  define RMODE_CtoML(m)	INT_CtoML(m)
#  define RMODE_MLtoC(m)	INT_MLtoC(m)
#else
#  define RMODE_CtoML(m)						\
      (RMODE_EQ(m, FE_TONEAREST) ? INT_CtoML(0)				\
	: (RMODE_EQ(m, FE_TOWARDZERO) ? INT_CtoML(1)			\
	  : (RMODE_EQ(m, FE_UPWARD) ? INT_CtoML(2) : INT_CtoML(3))))
PVT fe_rnd_mode_t ModeMap[4] = {
	FE_TONEAREST, FE_TOWARDZERO, FE_UPWARD, FE_DOWNWARD
    };
#  define RMODE_MLtoC(m)	ModeMap[INT_MLtoC(m)]
#endif
#endif /* !NO_ROUNDING_MODE_CTL */

/* _ml_Math_ctlrndmode : int option -> int
 *
 * Get/set the rounding mode; the values are interpreted as follows:
 *
 *	0	To nearest
 *	1	To zero
 *	2	To +Inf
 *	3	To -Inf
 */
ml_val_t _ml_Math_ctlrndmode (ml_state_t *msp, ml_val_t arg)
{
#ifdef NO_ROUNDING_MODE_CTL
    return RAISE_ERROR(msp, "Rounding mode control not supported");

#else
    if (arg == OPTION_NONE) {
	fe_rnd_mode_t	res = fegetround();
	return RMODE_CtoML(res);
    }
    else {
	fe_rnd_mode_t	m = RMODE_MLtoC(OPTION_get(arg));
	fe_rnd_mode_t	res = fesetround(m);
	return RMODE_CtoML(res);
    }
#endif

} /* end of _ml_Math_ctlrndmode */

