/* ml-objects.h
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *
 * Macros and routines for allocating heap objects.
 */

#ifndef _ML_OBJECTS_
#define _ML_OBJECTS_

#ifndef _ML_BASE_
#include "ml-base.h"
#endif

#ifndef _ML_VALUES_
#include "ml-values.h"
#endif

#ifndef _ML_STATE_
#include "ml-state.h"
#endif

#ifndef _TAGS_
#include "tags.h"
#endif

/* extract info from objects */
#define OBJ_DESC(OBJ)		REC_SEL((OBJ), -1)
#define OBJ_LEN(OBJ)		GET_LEN(OBJ_DESC(OBJ))
#define OBJ_TAG(OBJ)		GET_TAG(OBJ_DESC(OBJ))


/** The size of an ML record in bytes (including descriptor) **/
#define REC_SZB(n)	(((n)+1)*sizeof(ml_val_t))


/** heap allocation macros **/

#define ML_AllocWrite(msp, i, x)	((((msp)->ml_allocPtr))[(i)] = (x))

#define ML_Alloc(msp, n)	(			\
    ((msp)->ml_allocPtr += ((n)+1)),			\
    PTR_CtoML((msp)->ml_allocPtr - (n)))

#define REF_ALLOC(msp, r, a)	{			\
	ml_state_t	*__msp = (msp);			\
	ml_val_t	*__p = __msp->ml_allocPtr;	\
	*__p++ = DESC_ref;				\
	*__p++ = (a);					\
	(r) = PTR_CtoML(__msp->ml_allocPtr + 1);	\
	__msp->ml_allocPtr = __p;			\
    }

#define REC_ALLOC1(msp, r, a)	{				\
	ml_state_t	*__msp = (msp);				\
	ml_val_t	*__p = __msp->ml_allocPtr;		\
	*__p++ = MAKE_DESC(1, DTAG_record);			\
	*__p++ = (a);						\
	(r) = PTR_CtoML(__msp->ml_allocPtr + 1);		\
	__msp->ml_allocPtr = __p;				\
    }

#define REC_ALLOC2(msp, r, a, b)	{			\
	ml_state_t	*__msp = (msp);				\
	ml_val_t	*__p = __msp->ml_allocPtr;		\
	*__p++ = DESC_pair;					\
	*__p++ = (a);						\
	*__p++ = (b);						\
	(r) = PTR_CtoML(__msp->ml_allocPtr + 1);		\
	__msp->ml_allocPtr = __p;				\
    }

#define REC_ALLOC3(msp, r, a, b, c)	{			\
	ml_state_t	*__msp = (msp);				\
	ml_val_t	*__p = __msp->ml_allocPtr;		\
	*__p++ = MAKE_DESC(3, DTAG_record);			\
	*__p++ = (a);						\
	*__p++ = (b);						\
	*__p++ = (c);						\
	(r) = PTR_CtoML(__msp->ml_allocPtr + 1);		\
	__msp->ml_allocPtr = __p;				\
    }

#define REC_ALLOC4(msp, r, a, b, c, d)	{			\
	ml_state_t	*__msp = (msp);				\
	ml_val_t	*__p = __msp->ml_allocPtr;		\
	*__p++ = MAKE_DESC(4, DTAG_record);			\
	*__p++ = (a);						\
	*__p++ = (b);						\
	*__p++ = (c);						\
	*__p++ = (d);						\
	(r) = PTR_CtoML(__msp->ml_allocPtr + 1);		\
	__msp->ml_allocPtr = __p;				\
    }

#define REC_ALLOC5(msp, r, a, b, c, d, e)	{		\
	ml_state_t	*__msp = (msp);				\
	ml_val_t	*__p = __msp->ml_allocPtr;		\
	*__p++ = MAKE_DESC(5, DTAG_record);			\
	*__p++ = (a);						\
	*__p++ = (b);						\
	*__p++ = (c);						\
	*__p++ = (d);						\
	*__p++ = (e);						\
	(r) = PTR_CtoML(__msp->ml_allocPtr + 1);		\
	__msp->ml_allocPtr = __p;				\
    }

#define REC_ALLOC6(msp, r, a, b, c, d, e, f)	{		\
	ml_state_t	*__msp = (msp);				\
	ml_val_t	*__p = __msp->ml_allocPtr;		\
	*__p++ = MAKE_DESC(6, DTAG_record);			\
	*__p++ = (a);						\
	*__p++ = (b);						\
	*__p++ = (c);						\
	*__p++ = (d);						\
	*__p++ = (e);						\
	*__p++ = (f);						\
	(r) = PTR_CtoML(__msp->ml_allocPtr + 1);		\
	__msp->ml_allocPtr = __p;				\
    }

#define SEQHDR_ALLOC(msp, r, desc, data, len)	{		\
	ml_state_t	*__msp = (msp);				\
	ml_val_t	*__p = __msp->ml_allocPtr;		\
	*__p++ = (desc);					\
	*__p++ = (data);					\
	*__p++ = INT_CtoML(len);				\
	(r) = PTR_CtoML(__msp->ml_allocPtr + 1);		\
	__msp->ml_allocPtr = __p;				\
    }

#ifdef ALIGN_REALDS
#define REAL64_ALLOC(msp, r, d) {				\
	ml_state_t	*__msp = (msp);				\
	ml_val_t	*__p = __msp->ml_allocPtr;		\
	__p = (ml_val_t *)((Addr_t)__p | WORD_SZB);		\
	*__p++ = DESC_reald;					\
	(r) = PTR_CtoML(__p);					\
	*(double *)__p = (d);					\
	__p += REALD_SZW;					\
	__msp->ml_allocPtr = __p;				\
    }
#else
#define REAL64_ALLOC(msp, r, d) {				\
	ml_state_t	*__msp = (msp);				\
	ml_val_t	*__p = __msp->ml_allocPtr;		\
	__p = (ml_val_t *)((Addr_t)__p | WORD_SZB);		\
	(r) = PTR_CtoML(__p);					\
	*(double *)__p = (d);					\
	__p += REALD_SZW;					\
	__msp->ml_allocPtr = __p;				\
    }
#endif

#define EXN_ALLOC(msp, ex, id, val, where) \
	REC_ALLOC3(msp, ex, id, val, where)

/** Boxed word values **/
#define WORD_MLtoC(w)		(*PTR_MLtoC(Word_t, w))
#define WORD_ALLOC(msp, p, w)	{				\
	ml_state_t	*__msp = (msp);				\
	ml_val_t	*__p = __msp->ml_allocPtr;		\
	*__p++ = MAKE_DESC(1, DTAG_raw32);			\
	*__p++ = (ml_val_t)(w);					\
	(p) = PTR_CtoML(__msp->ml_allocPtr + 1);		\
	__msp->ml_allocPtr = __p;				\
    }
#define REC_SELWORD(p, i)	(*REC_SELPTR(Word_t, p, i))
#define INT32_MLtoC(i)		(*PTR_MLtoC(Int32_t, i))
#define INT32_ALLOC(msp, p, i)	WORD_ALLOC(msp, p, i)
#define REC_SELINT32(p, i)	(*REC_SELPTR(Int32_t, p, i))

/** ML lists **/
#define LIST_hd(p)		REC_SEL(p, 0)
#define LIST_tl(p)		REC_SEL(p, 1)
#define LIST_nil		INT_CtoML(0)
#define LIST_isNull(p)		((p) == LIST_nil)
#define LIST_cons(msp, r, a, b)	REC_ALLOC2(msp, r, a, b)

/** ML references **/
#define DEREF(r)		REC_SEL(r, 0)
#define ASSIGN(r, x)		(PTR_MLtoC(ml_val_t, r)[0] = (x))

/** ML options **/
#define OPTION_NONE             INT_CtoML(0)
#define OPTION_SOME(msp, r, a)  REC_ALLOC1(msp, r, a)
#define OPTION_get(r)		REC_SEL(r, 0)

/** external routines **/
extern ml_val_t ML_CString (ml_state_t *msp, const char *v);
extern ml_val_t ML_CStringList (ml_state_t *msp, char **strs);
extern ml_val_t ML_AllocString (ml_state_t *msp, int len);
extern ml_val_t ML_AllocCode (ml_state_t *msp, int len);
extern ml_val_t ML_AllocBytearray (ml_state_t *msp, int len);
extern ml_val_t ML_AllocRealdarray (ml_state_t *msp, int len);
extern ml_val_t ML_AllocArray (ml_state_t *msp, int len, ml_val_t initVal);
extern ml_val_t ML_AllocVector (ml_state_t *msp, int len, ml_val_t initVal);
extern ml_val_t ML_AllocRaw32 (ml_state_t *msp, int len);
extern void ML_ShrinkRaw32 (ml_state_t *msp, ml_val_t v, int nWords);
extern ml_val_t ML_AllocRaw64 (ml_state_t *msp, int len);

extern ml_val_t ML_SysConst (ml_state_t *msp, sysconst_tbl_t *tbl, int id);
extern ml_val_t ML_SysConstList (ml_state_t *msp, sysconst_tbl_t *tbl);
extern ml_val_t ML_AllocCData (ml_state_t *msp, int nbytes);
extern ml_val_t ML_CData (ml_state_t *msp, void *data, int nbytes);

extern ml_val_t BuildLiterals (ml_state_t *msp, Byte_t *lits, int len);

extern ml_val_t _ML_string0[];
extern ml_val_t _ML_vector0[];
#define ML_string0	PTR_CtoML(_ML_string0+1)
#define ML_vector0	PTR_CtoML(_ML_vector0+1)

#endif /* !_ML_OBJECTS_ */
