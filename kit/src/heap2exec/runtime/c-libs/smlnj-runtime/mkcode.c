/* mkcode.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 */

#include "cache-flush.h"
#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"


/* _ml_RunT_mkcode : Word8Vector.vector * string option -> (Word8Vector.vector * (unit -> unit))
 *
 * Turn a byte vector into a code-string, and a bootable closure.  This means
 * copying it to code space and flushing the I-cache.
 */
ml_val_t _ml_RunT_mkcode (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t    argCode = REC_SEL(arg,0);
    ml_val_t    argTag = REC_SEL(arg,1);

    Word_t	nbytes = OBJ_LEN(argCode);
    ml_val_t	code, closure, res;

    if (argTag == OPTION_NONE) {
      /* this code object should already have a comment */

	code = ML_AllocCode (msp, nbytes);
	arg = REC_SEL(msp->ml_arg, 1); /* in case ML_AllocCode caused GC */
	argCode = REC_SEL(arg,0);
      
      /* copy the string into the code object */
	memcpy (PTR_MLtoC(char, code), PTR_MLtoC(char, argCode), nbytes);
      
	FlushICache (PTR_MLtoC(char, code), nbytes);
      
	REC_ALLOC1(msp, closure, PTR_CtoML(PTR_MLtoC(ml_val_t, code) + 1));
	REC_ALLOC2(msp, res, argCode, closure);
      
	return res;
    }
    else {
      /* this object needs to be tagged */
	ml_val_t	str = OPTION_get(argTag);
	int		strLen = OBJ_LEN(str);
	int		padLen, extraLen;

      /* We use one byte for the length, so the longest string is 255
       * characters.  We need padding so that the code + string +
       * length byte is WORD_SZB bytes.  The padding is inserted between
       * the code and the string.
       */
	if (strLen > 255)
	    strLen = 255;
	extraLen = strLen+1;  /* include byte for length */
	padLen = ROUNDUP(nbytes+extraLen, WORD_SZB) - (nbytes+extraLen);
	extraLen += padLen;

	code = ML_AllocCode (msp, nbytes+extraLen);
	arg = REC_SEL(msp->ml_arg, 1); /* in case ML_AllocCode caused GC */
	argCode = REC_SEL(arg,0);
	argTag = REC_SEL(arg,1);
	str = OPTION_get(argTag);
      
      /* copy the string into the code object */
	memcpy (PTR_MLtoC(char, code), PTR_MLtoC(char, argCode), nbytes);
	memcpy (PTR_MLtoC(char, code)+nbytes+padLen, PTR_MLtoC(char, str), strLen);
	*(PTR_MLtoC(Byte_t, code)+nbytes+extraLen-1) = (Byte_t)strLen;
      
	FlushICache (PTR_MLtoC(void, code), nbytes);
      
	REC_ALLOC1(msp, closure, PTR_CtoML(PTR_MLtoC(ml_val_t, code) + 1));
	REC_ALLOC2(msp, res, argCode, closure);
      
	return res;
    }

} /* end of _ml_RunT_mkcode */

