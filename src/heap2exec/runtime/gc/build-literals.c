/* build-literals.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-objects.h"
#include "heap.h"

/* codes for literals:
 *   0-251	a short string (code is length)
 *   252	a long string (next four bytes are length)
 *   253	a 32-bit float (not used yet)
 *   254	a 64-bit float
 *   255	a 128-bit float (not used yet)
 */
#define STR0		((Byte_t)0)
#define STR1		((Byte_t)1)
#define LONG_STR	((Byte_t)252)
#define REAL32		((Byte_t)253)
#define REAL64		((Byte_t)254)
#define REAL128		((Byte_t)255)


#define ALLOC_STRING(ap, res, n) {						\
	arena_t	*__ap = (ap);							\
	int	__n = (n);							\
	int	__nwords = BYTES_TO_WORDS(__n);					\
	int	__allocSz = (((__n & 0x3) == 0) ? __nwords+1 : __nwords);	\
	*(__ap->nextw++) = MAKE_DESC(__n, DTAG_string);				\
	(res) = PTR_CtoML(__ap->nextw);						\
	__ap->nextw += __allocSz;						\
    }

/* BuildLiterals:
 *
 * NOTE: we allocate all of the objects in the first generation, and allocate
 * the vector of literals in the allocation space.
 */
ml_val_t BuildLiterals (ml_state_t *msp, ml_val_t start)
{
    int		nLits = REC_SELINT(start, 0);
    Byte_t	b, *p = PTR_MLtoC(Byte_t, start) + WORD_SZB;
    int		i;
    ml_val_t	*lits, lit, res;
    gen_t	*gen1 = msp->ml_heap->gen[0];
    arena_t	*ap = gen1->arena[STRING_INDX];

/** Should check the amount of space needed and the amount that is available **/

    lits = NEW_VEC(ml_val_t, nLits);
    for (i = 0;  i < nLits;  i++) {
	switch (b = (*p++)) {
	  case STR0:
	    lit = ML_string0;
	    break;
#ifdef CHAR_TABLE
	  case STR1:
	    lit = CharTbl[*p++];
	    break;
#endif
	  case LONG_STR: {
	    int		len;
	    len = p[0]<<24 + p[1]<<16 + p[2]<<8 + p[3];
	    p += 4;
	    ALLOC_STRING(ap, lit, len);
	    strncpy (PTR_MLtoC(char, lit), p, len);
	    p += len;
	    } break;
	  case REAL32:
	    Die ("Real32 not supported");
	    break;
	  case REAL64: {
#ifdef ALIGN_REALDS
	  /* Force REALD_SZB alignment (descriptor is off by one word) */
	    ap->nextw = (ml_val_t *)((Addr_t)(ap->nextw) | WORD_SZB);
#endif
	    *ap->nextw++ = DESC_reald;
	    memcpy (ap->nextw, p, REALD_SZB);
	    ap->nextw += REALD_SZB;
	    } break;
	  case REAL128:
	    Die ("Real128 not supported");
	    break;
	  default:
	    ASSERT ((0 <= b) && (b < LONG_STR));
	    ALLOC_STRING(ap, lit, b);
	    strncpy (PTR_MLtoC(char, lit), p, b);
	    p += b;
	    break;
	}
	lits[i] = lit;
    }

    ML_AllocWrite(msp, 0, MAKE_DESC(nLits, DTAG_record));
    for (i = 0;  i < nLits;  i++) {
	ML_AllocWrite(msp, i+1, lits[i]);
    }
    res = ML_Alloc(msp, nLits);

    FREE(lits);

    return res;

} /* end of BuildLiterals */

