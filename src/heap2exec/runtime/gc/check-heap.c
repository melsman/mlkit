/* check-heap.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "card-map.h"
#include "heap.h"
#include "c-globals-tbl.h"

/* local routines */
PVT void CheckRecordArena (arena_t *ap);
PVT void CheckPairArena (arena_t *ap);
PVT void CheckStringArena (arena_t *ap);
PVT void CheckArrayArena (arena_t *ap, card_map_t *cm);
PVT int CheckPtr (ml_val_t *p, ml_val_t w, int srcGen, int srcKind);

PVT int		ErrCount = 0;

#define ERROR	{					\
	if (++ErrCount > 100) {				\
	    Die("CheckHeap: too many errors\n");	\
	}						\
    }


/* CheckHeap:
 *
 * Check the heap for consistency after a garbage collection (or blast out).
 */
void CheckHeap (heap_t *heap, int maxSweptGen)
{
    int		i, j;

    ErrCount = 0;

    SayDebug ("Checking heap (%d generations) ...\n", maxSweptGen);
    for (i = 0;  i < maxSweptGen; i++) {
	gen_t	*g = heap->gen[i];

	CheckRecordArena (g->arena[RECORD_INDX]);
	CheckPairArena (g->arena[PAIR_INDX]);
	CheckStringArena (g->arena[STRING_INDX]);
	CheckArrayArena (g->arena[ARRAY_INDX], g->dirty);
    }
    SayDebug ("... done\n");

    if (ErrCount > 0)
	Die ("CheckHeap --- inconsistent heap\n");

} /* end of CheckHeap */

/* CheckRecordArena:
 *
 * Check the record arena.
 */
PVT void CheckRecordArena (arena_t *ap)
{
    ml_val_t	*p, *stop, desc, w;
    int		i, len;
    int		gen = EXTRACT_GEN(ap->id);

    if (! isACTIVE(ap))
	return;

    SayDebug ("  records [%d]: [%#x..%#x:%#x)\n",
	gen, ap->tospBase, ap->nextw, ap->tospTop);

    p = ap->tospBase;
    stop = ap->nextw;
    while (p < stop) {
	desc = *p++;
	if (! isDESC(desc)) {
	    ERROR;
	    SayDebug (
		"** @%#x: expected descriptor, but found %#x in record arena\n",
		p-1, desc);
	    return;
	}
	switch (GET_TAG(desc)) {
	  case DTAG_record:
	    len = GET_LEN(desc);
	    break;
	  default:
	    ERROR;
	    SayDebug ("** @%#x: strange tag (%#x) in record arena\n",
		p-1, GET_TAG(desc));
	    return;
	} /* end of switch */
	for (i = 0;  i < len;  i++, p++) {
	    w = *p;
	    if (isDESC(w)) {
		ERROR;
		SayDebug (
		    "** @%#x: unexpected descriptor %#x in slot %d of %d\n",
		    p, w, i, GET_LEN(desc));
		return;
	    }
	    else if (isBOXED(w)) {
		CheckPtr(p, w, gen, OBJC_record);
	    }
	}
    }

} /* end of CheckRecordArena */

/* CheckPairArena:
 */
PVT void CheckPairArena (arena_t *ap)
{
    ml_val_t	*p, *stop, w;
    int		gen = EXTRACT_GEN(ap->id);

    if (! isACTIVE(ap))
	return;

    SayDebug ("  pairs [%d]: [%#x..%#x:%#x)\n",
	gen, ap->tospBase, ap->nextw, ap->tospTop);

    p = ap->tospBase + 2;
    stop = ap->nextw;
    while (p < stop) {
	w = *p++;
	if (isDESC(w)) {
	    ERROR;
	    SayDebug (
		"** @%#x: unexpected descriptor %#x in pair arena\n",
		p-1, w);
	    return;
	}
	else if (isBOXED(w)) {
	    CheckPtr(p, w, gen, OBJC_pair);
	}
    }

} /* end of CheckPairArena */

/* CheckStringArena:
 *
 * Check a string arena for consistency.
 */
PVT void CheckStringArena (arena_t *ap)
{
    ml_val_t	*p, *stop, *prevDesc, desc;
    int		len;
    int		gen = EXTRACT_GEN(ap->id);

    if (! isACTIVE(ap))
	return;

    SayDebug ("  strings [%d]: [%#x..%#x:%#x)\n",
	gen, ap->tospBase, ap->nextw, ap->tospTop);

    p = ap->tospBase;
    stop = ap->nextw;
    prevDesc = NIL(ml_val_t *);
    while (p < stop) {
	desc = *p++;
	if (! isDESC(desc)) {
#ifdef ALIGN_REALDS
	    ml_val_t	next = *p;
	    if ((! isDESC(next))
	    || ((GET_TAG(next) != DTAG_reald)
	    &&  (GET_TAG(next) != DTAG_realdarray))) {
#endif
		ERROR;
		SayDebug (
		    "** @%#x: expected descriptor, but found %#x in string arena\n",
		    p-1, desc);
		if (prevDesc != NIL(ml_val_t *))
	            SayDebug ("   previous string started @ %#x\n", prevDesc);
		return;
#ifdef ALIGN_REALDS
	    }
	    else {
	      /* the bogus descriptor is alignment padding */
		desc = next;  p++;
	    }
#endif
	}
	switch (GET_TAG(desc)) {
	  case DTAG_string:
	    len = GET_STR_LEN(desc);
	  /* include the 0 termination bytes */
	    if ((GET_LEN(desc) & (WORD_SZB-1)) == 0) len++;
	    break;
	  case DTAG_bytearray:
	    len = GET_STR_LEN(desc);
	    break;
	  case DTAG_reald:
	    len = REALD_SZW;
	    break;
	  case DTAG_realdarray:
	    len = GET_REALDARR_LEN(desc);
	    break;
	  default:
	    ERROR;
	    SayDebug ("** @%#x: strange tag (%#x) in string arena\n",
		p-1, GET_TAG(desc));
	    return;
	} /* end of switch */
	prevDesc = p-1;
	p += len;
    }

} /* end of CheckStringArena */

/* CheckArrayArena:
 */
PVT void CheckArrayArena (arena_t *ap, card_map_t *cm)
{
    ml_val_t	*p, *stop, desc, w;
    int		i, j, len;
    int		gen = EXTRACT_GEN(ap->id);

    if (! isACTIVE(ap))
	return;

    SayDebug ("  arrays [%d]: [%#x..%#x:%#x)\n",
	gen, ap->tospBase, ap->nextw, ap->tospTop);

    p = ap->tospBase;
    stop = ap->nextw;
    while (p < stop) {
	desc = *p++;
	if (! isDESC(desc)) {
	    ERROR;
	    SayDebug (
		"** @%#x: expected descriptor, but found %#x in array arena\n",
		p-1, desc);
	    return;
	}
	switch (GET_TAG(desc)) {
	  case DTAG_array:
	    len = GET_LEN(desc);
	    break;
	  case DTAG_special:
	    len = 1;
	    break;
	  default:
	    ERROR;
	    SayDebug ("** @%#x: strange tag (%#x) in array arena\n",
		p-1, GET_TAG(desc));
	    return;
	} /* end of switch */
	for (i = 0;  i < len;  i++, p++) {
	    w = *p;
	    if (isDESC(w)) {
		ERROR;
		SayDebug (
		    "** @%#x: unexpected descriptor %#x in array slot %d of %d\n",
		    p, w, i, GET_LEN(desc));
		for (p -= (i+1), j = 0;  j <= len;  j++, p++) {
		    SayDebug ("  %#x: %#10x\n", p, *p);
		}
		return;
	    }
	    else if (isBOXED(w)) {
		CheckPtr(p, w, gen, OBJC_array);
	    }
	}
    }

} /* end of CheckArrayArena */

/* CheckPtr:
 */
PVT int CheckPtr (ml_val_t *p, ml_val_t w, int srcGen, int srcKind)
{
    aid_t	aid = ADDR_TO_PAGEID(BIBOP, w);
    int		dstGen = EXTRACT_GEN(aid);

    switch (EXTRACT_OBJC(aid)) {
      case OBJC_record:
      case OBJC_pair:
      case OBJC_string:
      case OBJC_array:
	if (dstGen < srcGen) {
	    if (srcKind != OBJC_array) {
		ERROR;
	        SayDebug (
		    "** @%#x: reference to younger object @%#x (gen = %d)\n",
		    p, w, dstGen);
	    }
	}
	break;
      case OBJC_bigobj:
	break;
      case OBJC_new:
	ERROR;
	SayDebug ("** @%#x: unexpected new-space reference\n", p);
	dstGen = MAX_NUM_GENS;
	break;
      default:
	if (aid == AID_UNMAPPED) {
	    if (AddrToCSymbol(w) == NIL(const char *)) {
		ERROR;
		SayDebug (
		    "** @%#x: reference to unregistered external address %#x\n",
		    p, w);
	    }
	    dstGen = MAX_NUM_GENS;
	}
	else Die("bogus object class in BIBOP\n");
	break;
    } /* end of switch */

    return dstGen;

} /* end of CheckPtr */

