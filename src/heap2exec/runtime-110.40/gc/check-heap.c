/* check-heap.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "card-map.h"
#include "heap.h"
#include "c-globals-tbl.h"

#ifndef CHECK_HEAP
#  error CHECK_HEAP must be defined too
#endif

/* local routines */
PVT void CheckRecordArena (arena_t *ap);
PVT void CheckPairArena (arena_t *ap);
PVT void CheckStringArena (arena_t *ap);
PVT void CheckArrayArena (arena_t *ap, card_map_t *cm);
PVT int CheckPtr (ml_val_t *p, ml_val_t w, int srcGen, int srcKind, int dstKind);

PVT int		ErrCount = 0;

/* CheckPtr dstKind values */
#define OBJC_NEWFLG	(1 << OBJC_new)
#define OBJC_RECFLG	(1 << OBJC_record)
#define OBJC_PAIRFLG	(1 << OBJC_pair)
#define OBJC_STRFLG	(1 << OBJC_string)
#define OBJC_ARRFLG	(1 << OBJC_array)
#define OBJC_any	\
	(OBJC_NEWFLG|OBJC_RECFLG|OBJC_PAIRFLG|OBJC_STRFLG|OBJC_ARRFLG)

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
		    CheckPtr(p, w, gen, OBJC_record, OBJC_any);
		}
	    }
	    break;
	  case DTAG_arr_hdr:
	  case DTAG_vec_hdr:
	    switch (GET_LEN(desc)) {
	      case SEQ_poly:
		if (GET_TAG(desc) == DTAG_arr_hdr)
		    CheckPtr (p, *p, gen, OBJC_record, OBJC_ARRFLG);
		else
		    CheckPtr (p, *p, gen, OBJC_record, OBJC_RECFLG|OBJC_PAIRFLG);
		break;
	      case SEQ_word8:
	      case SEQ_word16:
	      case SEQ_word31:
	      case SEQ_word32:
	      case SEQ_real32:
	      case SEQ_real64:
		CheckPtr (p, *p, gen, OBJC_record, OBJC_STRFLG);
		break;
	      default:
		ERROR;
		SayDebug ("** @%#x: strange sequence kind %d in record arena\n",
		    p-1, GET_LEN(desc));
		return;
	    }
	    if (! isUNBOXED(p[1])) {
		ERROR;
		SayDebug ("** @%#x: sequence header length field not an in (%#x)\n",
		    p+1, p[1]);
	    }
	    p += 2;
	    break;
	  default:
	    ERROR;
	    SayDebug ("** @%#x: strange tag (%#x) in record arena\n",
		p-1, GET_TAG(desc));
	    return;
	} /* end of switch */
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
	    CheckPtr(p, w, gen, OBJC_pair, OBJC_any);
	}
    }

} /* end of CheckPairArena */

/* CheckStringArena:
 *
 * Check a string arena for consistency.
 */
PVT void CheckStringArena (arena_t *ap)
{
    ml_val_t	*p, *stop, *prevDesc, desc, next;
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
	if (isDESC(desc)) {
	    switch (GET_TAG(desc)) {
	      case DTAG_raw32:
	      case DTAG_raw64:
		len = GET_LEN(desc);
		break;
	      default:
		ERROR;
		SayDebug ("** @%#x: strange tag (%#x) in string arena\n",
		    p-1, GET_TAG(desc));
		if (prevDesc != NIL(ml_val_t *))
		    SayDebug ("   previous string started @ %#x\n", prevDesc);
		return;
	    }
	    prevDesc = p-1;
	    p += len;
	}
#ifdef ALIGN_REALDS
	else if ((desc == 0) && (((Addr_t)p & WORD_SZB) != 0))
	  /* assume this is alignment padding */
	    continue;
#endif
	else {
	    ERROR;
	    SayDebug (
		"** @%#x: expected descriptor, but found %#x in string arena\n",
		p-1, desc);
	    if (prevDesc != NIL(ml_val_t *))
	        SayDebug ("   previous string started @ %#x\n", prevDesc);
	    return;
	}
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
	  case DTAG_arr_data:
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
		CheckPtr(p, w, gen, OBJC_array, OBJC_any);
	    }
	}
    }

} /* end of CheckArrayArena */

/* CheckPtr:
 */
PVT int CheckPtr (ml_val_t *p, ml_val_t w, int srcGen, int srcKind, int dstKind)
{
    aid_t	aid = ADDR_TO_PAGEID(BIBOP, w);
    int		dstGen = EXTRACT_GEN(aid);
    int		objc = EXTRACT_OBJC(aid);

    switch (objc) {
      case OBJC_record:
      case OBJC_pair:
      case OBJC_string:
      case OBJC_array:
	if (!(dstKind & (1 << objc))) {
	    ERROR;
	    SayDebug (
		"** @%#x: sequence data kind mismatch (expected %d, found %d)\n",
		p, dstKind, objc);
	}
	if (dstGen < srcGen) {
	    if (srcKind != OBJC_array) {
		ERROR;
	        SayDebug (
		    "** @%#x: reference to younger object @%#x (gen = %d)\n",
		    p, w, dstGen);
	    }
	}
	if ((objc != OBJC_pair) && (! isDESC(((ml_val_t *)w)[-1]))) {
	    ERROR;
	    SayDebug ("** @%#x: reference into object middle @#x\n", p, w);
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

