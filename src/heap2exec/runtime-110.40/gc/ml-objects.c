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

PVT bool_t ShrinkCheck (arena_t *ap, Word_t reqSzB);


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

	res = ML_AllocRaw32 (msp, n);
      /* zero the last word to allow fast (word) string comparisons, and to
       * guarantee 0 termination.
       */
	PTR_MLtoC(Word_t, res)[n-1] = 0;
	strcpy (PTR_MLtoC(char, res), v);

	SEQHDR_ALLOC (msp, res, DESC_string, res, len);

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
    int		nwords = BYTES_TO_WORDS(len+1);
    ml_val_t	res;

    ASSERT(len > 0);

    res = ML_AllocRaw32 (msp, nwords);

  /* zero the last word to allow fast (word) string comparisons, and to
   * guarantee 0 termination.
   */
    PTR_MLtoC(Word_t, res)[nwords-1] = 0;

    SEQHDR_ALLOC (msp, res, DESC_string, res, len);

    return res;

} /* end of ML_AllocString. */

/* ML_AllocRaw32:
 *
 * Allocate an uninitialized chunk of raw32 data.
 */
ml_val_t ML_AllocRaw32 (ml_state_t *msp, int nwords)
{
    ml_val_t	desc = MAKE_DESC(nwords, DTAG_raw32);
    ml_val_t	res;
    Word_t	szb;

    ASSERT(nwords > 0);

    if (nwords > SMALL_OBJ_SZW) {
	arena_t	*ap = msp->ml_heap->gen[0]->arena[STRING_INDX];

	szb = WORD_SZB*(nwords + 1);
	BEGIN_CRITICAL_SECT(MP_GCGenLock)
	    IFGC (ap, szb+msp->ml_heap->allocSzB) {
	      /* we need to do a GC */
		ap->reqSizeB += szb;
		RELEASE_LOCK(MP_GCGenLock);
		    InvokeGC (msp, 1);
		    if (ShrinkCheck (ap, szb))
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

} /* end of ML_AllocRaw32. */

/* ML_ShrinkRaw32:
 *
 * Shrink a freshly allocated Raw32 vector.  This is used by the input routines
 * that must allocate space for input that may be excessive.
 */
void ML_ShrinkRaw32 (ml_state_t *msp, ml_val_t v, int nWords)
{
    int		oldNWords = OBJ_LEN(v);

    if (nWords == oldNWords)
	return;

    ASSERT((nWords > 0) && (nWords < oldNWords));

    if (oldNWords > SMALL_OBJ_SZW) {
	arena_t	*ap = msp->ml_heap->gen[0]->arena[STRING_INDX];
	ASSERT(ap->nextw - oldNWords == PTR_MLtoC(ml_val_t, v)); 
	ap->nextw -= (oldNWords - nWords);
    }
    else {
	ASSERT(msp->ml_allocPtr - oldNWords == PTR_MLtoC(ml_val_t, v)); 
	msp->ml_allocPtr -= (oldNWords - nWords);
    }

    PTR_MLtoC(ml_val_t, v)[-1] = MAKE_DESC(nWords, DTAG_raw32);

} /* end of ML_ShrinkRaw32 */

/* ML_AllocRaw64:
 *
 * Allocate an uninitialized chunk of raw64 data.
 */
ml_val_t ML_AllocRaw64 (ml_state_t *msp, int nelems)
{
    int		nwords = DOUBLES_TO_WORDS(nelems);
    ml_val_t	desc = MAKE_DESC(nwords, DTAG_raw64);
    ml_val_t	res;
    Word_t	szb;

    if (nwords > SMALL_OBJ_SZW) {
	arena_t	*ap = msp->ml_heap->gen[0]->arena[STRING_INDX];
	szb = WORD_SZB*(nwords + 2);
	BEGIN_CRITICAL_SECT(MP_GCGenLock)
	  /* NOTE: we use nwords+2 to allow for the alignment padding */
	    IFGC (ap, szb+msp->ml_heap->allocSzB) {
	      /* we need to do a GC */
		ap->reqSizeB += szb;
		RELEASE_LOCK(MP_GCGenLock);
		    InvokeGC (msp, 1);
		    if (ShrinkCheck (ap, szb))
			InvokeGC (msp, 1);
		ACQUIRE_LOCK(MP_GCGenLock);
	    }
#ifdef ALIGN_REALDS
	  /* Force REALD_SZB alignment (descriptor is off by one word) */
#  ifdef CHECK_HEAP
	    if (((Addr_t)ap->nextw & WORD_SZB) == 0) {
		*(ap->nextw) = (ml_val_t)0;
		ap->nextw++;
	    }
#  else
	    ap->nextw = (ml_val_t *)(((Addr_t)ap->nextw) | WORD_SZB);
#  endif
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

} /* end of ML_AllocRaw64 */

/* ML_AllocCode:
 *
 * Allocate an uninitialized ML code object.  Assume that len > 1.
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
    ml_val_t	res;

    res = ML_AllocRaw32 (msp, nwords);

  /* zero the last word to allow fast (word) string comparisons, and to
   * guarantee 0 termination.
   */
    PTR_MLtoC(Word_t, res)[nwords-1] = 0;

    SEQHDR_ALLOC (msp, res, DESC_word8arr, res, len);

    return res;

} /* end of ML_AllocBytearray. */

/* ML_AllocRealdarray:
 *
 * Allocate an uninitialized ML realarray.  Assume that len > 0.
 */
ml_val_t ML_AllocRealdarray (ml_state_t *msp, int len)
{
    ml_val_t	res;

    res = ML_AllocRaw64 (msp, len);

    SEQHDR_ALLOC (msp, res, DESC_real64arr, res, len);

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
    ml_val_t	desc = MAKE_DESC(len, DTAG_arr_data);
    int		i;
    Word_t	szb;

    if (len > SMALL_OBJ_SZW) {
	arena_t	*ap = msp->ml_heap->gen[0]->arena[ARRAY_INDX];
	int	gcLevel = (isBOXED(initVal) ? 0 : -1);

	szb = WORD_SZB*(len + 1);
	BEGIN_CRITICAL_SECT(MP_GCGenLock)
#ifdef MP_SUPPORT
	  checkGC:;	/* the MP version jumps to here to recheck for GC */
#endif
	    if (! isACTIVE(ap)
	    || (AVAIL_SPACE(ap) <= szb+msp->ml_heap->allocSzB))
		gcLevel = 1;
	    if (gcLevel >= 0) {
	      /* we need to do a GC (and preserve initVal) */
		ml_val_t	root = initVal;
		ap->reqSizeB += szb;
		RELEASE_LOCK(MP_GCGenLock);
		    InvokeGCWithRoots (msp, gcLevel, &root, NIL(ml_val_t *));
		    if (ShrinkCheck(ap, szb))
			InvokeGCWithRoots (msp, 1, &root, NIL(ml_val_t *));
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

    for (p = PTR_MLtoC(ml_val_t, res), i = 0;  i < len; i++)
	*p++ = initVal;

    SEQHDR_ALLOC (msp, res, DESC_polyarr, res, len);

    return res;

} /* end of ML_AllocArray. */

/* ML_AllocVector:
 *
 * Allocate an ML vector, using the list initVal as an initializer.
 * Assume that len > 0.
 */
ml_val_t ML_AllocVector (ml_state_t *msp, int len, ml_val_t initVal)
{
    ml_val_t	desc = MAKE_DESC(len, DTAG_vec_data);
    ml_val_t	res, *p;

    if (len > SMALL_OBJ_SZW) {
      /* Since we want to avoid pointers from the 1st generation record space
       * into the allocation space, we need to do a GC (and preserve initVal)
       */
	arena_t		*ap = msp->ml_heap->gen[0]->arena[RECORD_INDX];
	ml_val_t	root = initVal;
	int		gcLevel = 0;
	Word_t		szb;

	szb = WORD_SZB*(len + 1);
	BEGIN_CRITICAL_SECT(MP_GCGenLock)
	    if (! isACTIVE(ap)
	    || (AVAIL_SPACE(ap) <= szb+msp->ml_heap->allocSzB))
		gcLevel = 1;
#ifdef MP_SUPPORT
	  checkGC:;	/* the MP version jumps to here to redo the GC */
#endif
	    ap->reqSizeB += szb;
	    RELEASE_LOCK(MP_GCGenLock);
	        InvokeGCWithRoots (msp, gcLevel, &root, NIL(ml_val_t *));
		if (ShrinkCheck(ap, szb))
		    InvokeGCWithRoots (msp, 1, &root, NIL(ml_val_t *));
	        initVal = root;
	    ACQUIRE_LOCK(MP_GCGenLock);
#ifdef MP_SUPPORT
	  /* check again to insure that we have sufficient space */
	    if (AVAIL_SPACE(ap) <= szb+msp->ml_heap->allocSzB)
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

    for (
	p = PTR_MLtoC(ml_val_t, res);
	initVal != LIST_nil;
	initVal = LIST_tl(initVal)
    )
	*p++ = LIST_hd(initVal);

    SEQHDR_ALLOC (msp, res, DESC_polyvec, res, len);

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
 * Allocate a 64-bit aligned raw data object (to store abstract C data).
 */
ml_val_t ML_AllocCData (ml_state_t *msp, int nbytes)
{
    ml_val_t	obj;

    obj = ML_AllocRaw64 (msp, (nbytes+7)>>2);

    return obj;

} /* end of ML_AllocCData */


/* ML_CData:
 *
 * Allocate a 64-bit aligned raw data object and initialize it to the given C data.
 */
ml_val_t ML_CData (ml_state_t *msp, void *data, int nbytes)
{
    ml_val_t	obj;

    if (nbytes == 0)
	return ML_unit;
    else {
	obj = ML_AllocRaw64 (msp, (nbytes+7)>>2);
	memcpy (PTR_MLtoC(void, obj), data, nbytes);

	return obj;
    }

} /* end of ML_CData */


/********** Local routines **********/

/* ShrinkCheck:
 *
 * This function checks to see if we need to do another GC to shrink the
 * heap.  It is a hack to work around a flaw in the sizing policy that can
 * lead to unbounded heap growth even when the live data is constant.
 */
PVT bool_t ShrinkCheck (arena_t *ap, Word_t reqSzB)
{
    if ((ap->tospSizeB > ap->maxSizeB)
    && (USED_SPACE(ap) + 2*reqSzB < ap->tospSizeB)) {
      /* here the arena has grown beyond the soft max, while it would be
       * possible to fit the twice request within the arena, so we force
       * another GC.
       */
	ap->reqSizeB = reqSzB;
	return TRUE;
    }
    else
	return FALSE;

} /* end of ShrinkCheck */
