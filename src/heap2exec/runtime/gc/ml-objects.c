/* ml-objects.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * Code to allocate and manipulate ML objects.
 *
 * MP Note: when invoking the GC, we add the requested size to reqSizeB,
 * so that multiple processors can request space at the same time.
 */

#include "ml-base.h"
#include "heap.h"
#include "ml-objects.h"
#include "ml-limits.h"
#include "ml-mp.h"

/* A macro to check for necessary GC; on MP systems, this needs to be
 * a loop, since other processors may steal the memory before the
 * checking processor can use it.
 */
#ifdef MP_SUPPORT
#define IFGC(ap, szb)	\
	while ((! isACTIVE(ap)) || (AVAIL_SPACE(ap) <= (szb)))
#else
#define IFGC(ap, szb)	\
	if ((! isACTIVE(ap)) || (AVAIL_SPACE(ap) <= (szb)))
#endif

#ifdef COLLECT_STATS
#define COUNT_ALLOC(msp, nbytes)	{	\
	heap_t		*__h = msp->ml_heap;	\
	CNTR_INCR(&(__h->numAlloc), (nbytes));	\
    }
#else
#define COUNT_ALLOC(msp, nbytes)	/* null */
#endif


/* ML_CString:
 *
 * Allocate an ML string using a C string as an initializer.  We assume
 * that the string is small and can be allocated in the allocation
 * arena.
 */
ml_val_t ML_CString (ml_state_t *msp, const char *v)
{
    int		len = ((v == NIL(char *)) ? 0 : strlen(v));

    if (len == 0)
	return ML_string0;
    else {
	int		n = BYTES_TO_WORDS(len+1);  /* count "\0" too */
	ml_val_t	res;

	ML_AllocWrite (msp, 0, MAKE_DESC(len, DTAG_string));
	ML_AllocWrite (msp, n, 0);  /* so word-by-word string equality works */
	res = ML_Alloc (msp, n);
	strcpy (PTR_MLtoC(char, res), v);
	return res;
    }

} /* end of ML_CString */

/* ML_CStringList:
 *
 * Given a NIL terminated array of char *, build a list of ML strings.
 */
ml_val_t ML_CStringList (ml_state_t *msp, char **strs)
{
/** NOTE: we should do something about possible GC!!! **/
    int		i;
    ml_val_t	p, s;

    for (i = 0;  strs[i] != NIL(char *);  i++)
	continue;

    p = LIST_nil;
    while (i-- > 0) {
	s = ML_CString(msp, strs[i]);
	LIST_cons(msp, p, s, p);
    }

    return p;

} /* end of ML_CStringList */

/* ML_AllocString:
 *
 * Allocate an uninitialized ML string of length > 0.  This string is
 * guaranteed to be padded to word size with 0 bytes, and to be 0 terminated.
 */
ml_val_t ML_AllocString (ml_state_t *msp, int len)
{
    int		nwords = BYTES_TO_WORDS(len);
    int		allocSz = (((len & 0x3) == 0) ? nwords+1 : nwords);
    ml_val_t	desc = MAKE_DESC(len, DTAG_string);
    ml_val_t	res;

    if (allocSz > SMALL_OBJ_SZW) {
	arena_t	*ap = msp->ml_heap->gen[0]->arena[STRING_INDX];

	BEGIN_CRITICAL_SECT(MP_GCGenLock)
	    IFGC (ap, (WORD_SZB*(allocSz + 1))+msp->ml_heap->allocSzB) {
	      /* we need to do a GC */
		ap->reqSizeB += WORD_SZB*(allocSz + 1);
		RELEASE_LOCK(MP_GCGenLock);
		    InvokeGC (msp, 1);
		ACQUIRE_LOCK(MP_GCGenLock);
	    }
	    *(ap->nextw++) = desc;
	    res = PTR_CtoML(ap->nextw);
	    ap->nextw += allocSz;
	END_CRITICAL_SECT(MP_GCGenLock)
	COUNT_ALLOC(msp, WORD_SZB*(allocSz + 1));
    }
    else {
	ML_AllocWrite (msp, 0, desc);
	res = ML_Alloc (msp, allocSz);
    }

  /* zero the last word to allow fast (word) string comparisons, and to
   * guarantee 0 termination.
   */
    PTR_MLtoC(Word_t, res)[allocSz-1] = 0;

    return res;

} /* end of ML_AllocString. */

/* ML_AllocCode:
 *
 * Allocate an uninitialized ML code string.  Assume that len > 1.
 */
ml_val_t ML_AllocCode (ml_state_t *msp, int len)
{
    heap_t	    *heap = msp->ml_heap;
    int		    allocGen = (heap->numGens < CODE_ALLOC_GEN)
			? heap->numGens
			: CODE_ALLOC_GEN;
    gen_t	    *gen = heap->gen[allocGen-1];
    bigobj_desc_t   *dp;

    BEGIN_CRITICAL_SECT(MP_GCGenLock)
	dp = BO_Alloc (heap, allocGen, len);
	ASSERT(dp->gen == allocGen);
	dp->next = gen->bigObjs[CODE_INDX];
	gen->bigObjs[CODE_INDX] = dp;
	dp->objc = CODE_INDX;
	COUNT_ALLOC(msp, len);
    END_CRITICAL_SECT(MP_GCGenLock)

    return PTR_CtoML(dp->obj);

} /* end of ML_AllocCode. */

/* ML_AllocBytearray:
 *
 * Allocate an uninitialized ML bytearray.  Assume that len > 0.
 */
ml_val_t ML_AllocBytearray (ml_state_t *msp, int len)
{
    int		nwords = BYTES_TO_WORDS(len);
    ml_val_t	desc = MAKE_DESC(len, DTAG_bytearray);
    ml_val_t	res;

    if (nwords > SMALL_OBJ_SZW) {
	arena_t	*ap = msp->ml_heap->gen[0]->arena[STRING_INDX];
	BEGIN_CRITICAL_SECT(MP_GCGenLock)
	    IFGC (ap, (WORD_SZB*(nwords + 1))+msp->ml_heap->allocSzB) {
	      /* we need to do a GC */
		ap->reqSizeB += WORD_SZB*(nwords + 1);
		RELEASE_LOCK(MP_GCGenLock);
		    InvokeGC (msp, 1);
		ACQUIRE_LOCK(MP_GCGenLock);
	    }
	    *(ap->nextw++) = desc;
	    res = PTR_CtoML(ap->nextw);
	    ap->nextw += nwords;
	END_CRITICAL_SECT(MP_GCGenLock)
	COUNT_ALLOC(msp, WORD_SZB*(nwords + 1));
    }
    else {
	ML_AllocWrite (msp, 0, desc);
	res = ML_Alloc (msp, nwords);
    }

    return res;

} /* end of ML_AllocBytearray. */

/* ML_AllocRealdarray:
 *
 * Allocate an uninitialized ML realarray.  Assume that len > 0.
 */
ml_val_t ML_AllocRealdarray (ml_state_t *msp, int len)
{
    int		nwords = DOUBLES_TO_WORDS(len);
    ml_val_t	desc = MAKE_DESC(len, DTAG_realdarray);
    ml_val_t	res;

    if (nwords > SMALL_OBJ_SZW) {
	arena_t	*ap = msp->ml_heap->gen[0]->arena[STRING_INDX];
	BEGIN_CRITICAL_SECT(MP_GCGenLock)
	  /* NOTE: we use nwords+2 to allow for the alignment padding */
	    IFGC (ap, (WORD_SZB*(nwords + 2))+msp->ml_heap->allocSzB) {
	      /* we need to do a GC */
		ap->reqSizeB += WORD_SZB*(nwords + 1);
		RELEASE_LOCK(MP_GCGenLock);
		    InvokeGC (msp, 1);
		ACQUIRE_LOCK(MP_GCGenLock);
	    }
#ifdef ALIGN_REALDS
	  /* Force REALD_SZB alignment (descriptor is off by one word) */
	    ap->nextw = (ml_val_t *)((Addr_t)(ap->nextw) | WORD_SZB);
#endif
	    *(ap->nextw++) = desc;
	    res = PTR_CtoML(ap->nextw);
	    ap->nextw += nwords;
	END_CRITICAL_SECT(MP_GCGenLock)
	COUNT_ALLOC(msp, WORD_SZB*(nwords + 1));
    }
    else {
#ifdef ALIGN_REALDS
      /* Force REALD_SZB alignment */
	msp->ml_allocPtr = (ml_val_t *)((Addr_t)(msp->ml_allocPtr) | WORD_SZB);
#endif
	ML_AllocWrite (msp, 0, desc);
	res = ML_Alloc (msp, nwords);
    }

    return res;

} /* end of ML_AllocRealdarray. */

/* ML_AllocArray:
 *
 * Allocate an ML array using initVal as an initial value.  Assume
 * that len > 0.
 */
ml_val_t ML_AllocArray (ml_state_t *msp, int len, ml_val_t initVal)
{
    ml_val_t	res, *p;
    ml_val_t	desc = MAKE_DESC(len, DTAG_array);

    if (len > SMALL_OBJ_SZW) {
	arena_t	*ap = msp->ml_heap->gen[0]->arena[ARRAY_INDX];
	int	gcLevel = (isBOXED(initVal) ? 0 : -1);

	BEGIN_CRITICAL_SECT(MP_GCGenLock)
	  checkGC:;	/* the MP version jumps to here to recheck for GC */
	    if (! isACTIVE(ap)
	    || (AVAIL_SPACE(ap) <= (WORD_SZB*(len + 1))+msp->ml_heap->allocSzB))
		gcLevel = 1;
	    if (gcLevel >= 0) {
	      /* we need to do a GC (and preserve initVal) */
		ml_val_t	root = initVal;
		ap->reqSizeB += WORD_SZB*(len + 1);
		RELEASE_LOCK(MP_GCGenLock);
		    InvokeGCWithRoots (msp, gcLevel, &root, NIL(ml_val_t *));
		    initVal = root;
		ACQUIRE_LOCK(MP_GCGenLock);
#ifdef MP_SUPPORT
	      /* check again to insure that we have sufficient space */
		gcLevel = -1;
		goto checkGC;
#endif
	    }
	    ASSERT(ap->nextw == ap->sweep_nextw);
	    *(ap->nextw++) = desc;
	    res = PTR_CtoML(ap->nextw);
	    ap->nextw += len;
	    ap->sweep_nextw = ap->nextw;
	END_CRITICAL_SECT(MP_GCGenLock)
	COUNT_ALLOC(msp, WORD_SZB*(len + 1));
    }
    else {
	ML_AllocWrite (msp, 0, desc);
	res = ML_Alloc (msp, len);
    }

    for (p = PTR_MLtoC(ml_val_t, res);  --len >= 0; )
	*p++ = initVal;

    return res;

} /* end of ML_AllocArray. */

/* ML_AllocVector:
 *
 * Allocate an ML vector, using the list initVal as an initializer.
 * Assume that len > 0.
 */
ml_val_t ML_AllocVector (ml_state_t *msp, int len, ml_val_t initVal)
{
    ml_val_t	desc = MAKE_DESC(len, DTAG_vector);
    ml_val_t	res, *p;

    if (len > SMALL_OBJ_SZW) {
      /* Since we want to avoid pointers from the 1st generation record space
       * into the allocation space, we need to do a GC (and preserve initVal)
       */
	arena_t		*ap = msp->ml_heap->gen[0]->arena[RECORD_INDX];
	ml_val_t	root = initVal;
	int		gcLevel = 0;

	BEGIN_CRITICAL_SECT(MP_GCGenLock)
	    if (! isACTIVE(ap)
	    || (AVAIL_SPACE(ap) <= (WORD_SZB*(len + 1))+msp->ml_heap->allocSzB))
		gcLevel = 1;
	  checkGC:;	/* the MP version jumps to here to redo the GC */
	    ap->reqSizeB += WORD_SZB*(len + 1);
	    RELEASE_LOCK(MP_GCGenLock);
	        InvokeGCWithRoots (msp, gcLevel, &root, NIL(ml_val_t *));
	        initVal = root;
	    ACQUIRE_LOCK(MP_GCGenLock);
#ifdef MP_SUPPORT
	  /* check again to insure that we have sufficient space */
	    if (AVAIL_SPACE(ap) <= (WORD_SZB*(len + 1))+msp->ml_heap->allocSzB)
		goto checkGC;
#endif
	    ASSERT(ap->nextw == ap->sweep_nextw);
	    *(ap->nextw++) = desc;
	    res = PTR_CtoML(ap->nextw);
	    ap->nextw += len;
	    ap->sweep_nextw = ap->nextw;
	END_CRITICAL_SECT(MP_GCGenLock)
	COUNT_ALLOC(msp, WORD_SZB*(len + 1));
    }
    else {
	ML_AllocWrite (msp, 0, desc);
	res = ML_Alloc (msp, len);
    }

    for (p = PTR_MLtoC(ml_val_t, res);  --len >= 0;  initVal = LIST_tl(initVal))
	*p++ = LIST_hd(initVal);

    return res;

} /* end of ML_AllocVector. */


/* ML_SysConst:
 *
 * Find the system constant with the given id in tbl, and allocate a pair
 * to represent it.  If the constant is not present, then return the
 * pair (~1, "<UNKNOWN>").
 */
ml_val_t ML_SysConst (ml_state_t *msp, sysconst_tbl_t *tbl, int id)
{
    ml_val_t	name, res;
    int		i;

    for (i = 0;  i < tbl->numConsts;  i++) {
	if (tbl->consts[i].id == id) {
	    name = ML_CString (msp, tbl->consts[i].name);
	    REC_ALLOC2 (msp, res, INT_CtoML(id), name);
	    return res;
	}
    }
  /* here, we did not find the constant */
    name = ML_CString (msp, "<UNKNOWN>");
    REC_ALLOC2 (msp, res, INT_CtoML(-1), name);
    return res;

} /* end of ML_SysConst */


/* ML_SysConstList:
 *
 * Generate a list of system constants from the given table.
 */
ml_val_t ML_SysConstList (ml_state_t *msp, sysconst_tbl_t *tbl)
{
    int		i;
    ml_val_t	name, sysConst, list;

/** should check for available heap space !!! **/
    for (list = LIST_nil, i = tbl->numConsts;  --i >= 0;  ) {
	name = ML_CString (msp, tbl->consts[i].name);
	REC_ALLOC2 (msp, sysConst, INT_CtoML(tbl->consts[i].id), name);
	LIST_cons(msp, list, sysConst, list);
    }

    return list;

} /* end of ML_SysConstList */


/* ML_CData:
 *
 * Allocate a Word8Vector.vector.
 */
ml_val_t ML_AllocCData (ml_state_t *msp, int nbytes)
{
    ml_val_t	obj;

    obj = ML_AllocString (msp, nbytes);

    return obj;

} /* end of ML_AllocCData */


/* ML_CData:
 *
 * Allocate a Word8Vector.vector and initialize it to the given C data.
 */
ml_val_t ML_CData (ml_state_t *msp, void *data, int nbytes)
{
    ml_val_t	obj;

    obj = ML_AllocString (msp, nbytes);
    memcpy (PTR_MLtoC(void, obj), data, nbytes);

    return obj;

} /* end of ML_CData */


/* ML_StringEq:
 * Test two ML strings for equality.
 */
bool_t ML_StringEq (ml_val_t a, ml_val_t b)
{
    int		l;

    if (a == b)
	return TRUE;
    else if ((l = OBJ_LEN(a)) != OBJ_LEN(b))
	return FALSE;
    else
	return (strncmp(PTR_MLtoC(char, a), PTR_MLtoC(char, b), l) == 0);

} /* end of ML_StringEq */

