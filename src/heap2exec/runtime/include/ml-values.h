/* ml-values.h
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *
 * Basic definitions for representing ML values in C.
 *
 *   INT_MLtoC(v)	-- convert an unboxed ML value to a Word_t.
 *   INT_CtoML(i)	-- convert a Word_t to an unboxed ML value.
 *   PTR_MLtoC(ty, v)	-- convert a boxed ML value to a (ty *).
 *   PTR_CtoML(p)	-- convert (Word_t *p) to an boxed ML value.
 */

#ifndef _ML_VALUES_
#define _ML_VALUES_

#ifndef _ML_BASE_
#include "ml-base.h"
#endif

/* typedef void *ml_val_t; */	/* defined in ml-base.h */

#ifdef BOXED1

#ifndef _ASM_
#define INT_MLtoC(n)		(((Int_t)(n)) >> 1)
#define INT_CtoML(n)		((ml_val_t)((n) << 1))
#define PTR_MLtoC(ty,p)		((ty *)(((Addr_t)(p))-1))
#define PTR_CtoML(p)		((ml_val_t)(((Addr_t)(p))+1))
#else
#define INT_CtoML(n)		((n)*2)
#endif /* !_ASM_ */

#else

#ifndef _ASM_

/* When the size of a C pointer differs from the size of an ML value, the
 * pointer cast should first convert to a address sized integer before
 * the actual cast.  This causes problems, however, for gcc when used in
 * a static initialization; hence the PTR_CAST macro.
 */
#ifdef SIZES_C64_ML32
#define PTR_CAST(ty, p)		((ty)(Addr_t)(p))
#else
#define PTR_CAST(ty, p)		((ty)(p))
#endif

#define INT_MLtoC(n)		(((Int_t)(n)) >> 1)
#define INT_CtoML(n)		((ml_val_t)(((n) << 1) + 1))
#define PTR_MLtoC(ty,p)		PTR_CAST(ty *, p)
#define PTR_CtoML(p)		PTR_CAST(ml_val_t, p)
#else
#define INT_CtoML(n)		(((n)*2)+1)
#endif /* !_ASM_ */

#endif /* BOXED1 */

#ifndef _ASM_

/* convert an ML pointer to an Addr_t value */
#define PTR_MLtoADDR(p)		((Addr_t)PTR_MLtoC(void, p))

/* ML record field selection */
#define REC_SEL(p, i)		((PTR_MLtoC(ml_val_t, p))[(i)])
#define REC_SELPTR(ty, p, i)	PTR_MLtoC(ty, REC_SEL(p, i))
#define REC_SELINT(p, i)	INT_MLtoC(REC_SEL(p, i))

/* Extract the code address from an ML closure */
#define GET_CODE_ADDR(c)	(REC_SEL(c, 0))

#endif /* !_ASM_ */


/** Some basic ML values **/
#define ML_unit			INT_CtoML(0)
#define ML_false		INT_CtoML(0)
#define ML_true			INT_CtoML(1)
#define ML_nil			INT_CtoML(0)

#endif /* !_ML_VALUES_ */

