/* cntr.h
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *
 * Large counters for large (> 2^31) values.
 */

#ifndef _CNTR_
#define _CNTR_

#define ONE_MILLION	1000000

typedef struct {
    Unsigned32_t	millions;
    Unsigned32_t	ones;
} cntr_t;

#define CNTR_INCR(cp, i)	{		\
	cntr_t		*__cp = (cp);		\
	__cp->ones += (i);			\
	while (__cp->ones > ONE_MILLION) {	\
	    __cp->ones -= ONE_MILLION;		\
	    __cp->millions++;			\
	}					\
    }

#define CNTR_INCR1(cp)	{			\
	cntr_t		*__cp = (cp);		\
	__cp->ones++;				\
	if (__cp->ones > ONE_MILLION) {		\
	    __cp->ones -= ONE_MILLION;		\
	    __cp->millions++;			\
	}					\
    }

#define CNTR_ZERO(cp)		{		\
	cntr_t		*__cp = (cp);		\
	__cp->ones = __cp->millions = 0;	\
    }

#define CNTR_TO_REAL(cp)			\
    (((double)((cp)->millions)*(double)ONE_MILLION) + (double)((cp)->ones))

/* Add cp2 to cp1 */
#define CNTR_ADD(cp1, cp2)	{		\
	cntr_t		*__cp1 = (cp1);		\
	cntr_t		*__cp2 = (cp2);		\
	__cp1->ones += __cp2->ones;		\
	if (__cp1->ones > ONE_MILLION) {	\
	    __cp1->ones -= ONE_MILLION;		\
	    __cp1->millions++;			\
	}					\
	__cp1->millions += __cp2->millions;	\
    }

#define CNTR_PERCENT(cp1, cp2)	((100.0*CNTR_TO_REAL(cp1)) / CNTR_TO_REAL(cp2))

#define CNTR_FPRINTF(f,cp,wid)	{					\
	cntr_t	*__cp = (cp);						\
	int	__w = (wid);						\
	if (__cp->millions > 0)						\
	    fprintf (f, "%*d%06d", __w-6, __cp->millions, __cp->ones);	\
	else								\
	    fprintf (f, "%*d", __w, __cp->ones);			\
    }

#endif /* !_CNTR_ */

