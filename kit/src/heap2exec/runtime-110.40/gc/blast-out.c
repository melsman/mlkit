/* blast-out.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 */

#include "ml-osdep.h"
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-heap-image.h"
#include "c-globals-tbl.h"
#include "addr-hash.h"
#include "gc.h"
#include "blast-out.h"
#include "heap-output.h"
#include "heap-io.h"

#define BLAST_ERROR	ML_unit

/* local routines */
PVT ml_val_t BlastUnboxed (ml_state_t *msp, ml_val_t obj);
PVT ml_val_t BlastHeap (ml_state_t *msp, ml_val_t obj, blast_res_t *info);
PVT ml_val_t AllocBlastData (ml_state_t *msp, Addr_t sizeB);


/* BlastOut:
 *
 * Linearize an ML object into a vector of bytes; return ML_unit on errors.
 */
ml_val_t BlastOut (ml_state_t *msp, ml_val_t obj)
{
    blast_res_t		res;
    int			gen;
    ml_val_t		blastedObj;

  /* Collect allocation space */
    InvokeGCWithRoots (msp, 0, &obj, NIL(ml_val_t *));

    gen = GetObjGen (obj);

    if (gen == -1) {
      /* unboxed */
	blastedObj = BlastUnboxed (msp, obj);
    }
    else { /* a regular ML object */
      /* do the blast GC */
/* DEBUG  CheckHeap (msp->ml_heap, msp->ml_heap->numGens); */
	res = BlastGC (msp, &obj, gen);

      /* blast out the image */
	blastedObj = BlastHeap (msp, obj, &res);

      /* repair the heap or finish the GC */
	BlastGC_FinishUp (msp, &res);

/* DEBUG CheckHeap (msp->ml_heap, res.maxGen); */
    }

    return blastedObj;

} /* end of BlastOut */


/* BlastUnboxed:
 *
 * Blast out an unboxed value.
 */
PVT ml_val_t BlastUnboxed (ml_state_t *msp, ml_val_t obj)
{
    ml_blast_hdr_t  blastHdr;
    int		    szB = sizeof(ml_image_hdr_t) + sizeof(ml_blast_hdr_t);
    ml_val_t	    blastedObj;
    writer_t	    *wr;

  /* allocate space for the object */
    blastedObj = AllocBlastData (msp, szB);
    wr = WR_OpenMem (PTR_MLtoC(Byte_t, blastedObj), szB);

    HeapIO_WriteImageHeader (wr, BLAST_UNBOXED);

    blastHdr.numArenas		= 0;
    blastHdr.numBOKinds		= 0;
    blastHdr.numBORegions	= 0;
    blastHdr.hasCode		= FALSE;
    blastHdr.rootObj		= obj;

    WR_Write(wr, &blastHdr, sizeof(blastHdr));

    if (WR_Error(wr))
	return ML_unit;
    else {
	WR_Free(wr);
	SEQHDR_ALLOC (msp, blastedObj, DESC_string, blastedObj, szB);
	return blastedObj;
    }

} /* end of BlastUnboxed */


/* BlastHeap:
 *
 * Blast out the heap image.
 */
PVT ml_val_t BlastHeap (ml_state_t *msp, ml_val_t obj, blast_res_t *info)
{
    heap_t		*heap = msp->ml_heap;
    int			maxGen = info->maxGen;
    Addr_t		totArenaSzB[NUM_ARENAS], totSzB;
    struct {
	Addr_t		    base;	/* the base address of the arena in the heap */
	Addr_t		    offset;	/* the relative position in the merged */
					/* arena. */
    }			adjust[MAX_NUM_GENS][NUM_ARENAS];
    heap_arena_hdr_t	*p, *arenaHdrs[NUM_OBJ_KINDS], *arenaHdrsBuf;
    int			arenaHdrSz, i, j, numArenas;
    ml_val_t	        blastedObj;
    writer_t		*wr;

  /* compute the arena offsets in the heap image */
    for (i = 0;  i < NUM_ARENAS; i++)
	totArenaSzB[i] = 0;
  /* the embedded literals go first */
    totArenaSzB[STRING_INDX] = BlastGC_AssignLitAddrs (info, STRING_INDX, 0);
/* DEBUG SayDebug("%d bytes of string literals\n", totArenaSzB[STRING_INDX]); */
    for (i = 0;  i < maxGen;  i++) {
	for (j = 0;  j < NUM_ARENAS;  j++) {
	    arena_t	*ap = heap->gen[i]->arena[j];
	    adjust[i][j].offset = totArenaSzB[j];
	    if (isACTIVE(ap)) {
/* DEBUG SayDebug("[%d][%d] base = %#x, nextw = %#x, %d bytes\n", */
/* DEBUG i, j, ap->tospBase, ap->nextw, (Addr_t)(ap->nextw) - (Addr_t)(ap->tospBase)); */
		totArenaSzB[j] += (Addr_t)(ap->nextw) - (Addr_t)(ap->tospBase);
		adjust[i][j].base = (Addr_t)(ap->tospBase);
	    }
	    else
		adjust[i][j].base = 0;
	}
    }
/* DEBUG for (i = 0;  i < NUM_ARENAS;  i++) SayDebug ("arena %d: %d bytes\n", i+1, totArenaSzB[i]); */
    /** WHAT ABOUT THE BIG OBJECTS??? **/

  /* Compute the total size of the blasted object */
    for (i = 0, numArenas = 0, totSzB = 0;  i < NUM_ARENAS;  i++) {
	if (totArenaSzB[i] > 0) {
	    numArenas++;
	    totSzB += totArenaSzB[i];
	}
    }
    totSzB += (sizeof(ml_image_hdr_t) + sizeof(ml_blast_hdr_t)
		+ (numArenas * sizeof(heap_arena_hdr_t)));
    /** COUNT SPACE FOR BIG OBJECTS **/

  /* include the space for the external symbols */
    totSzB += sizeof(extern_tbl_hdr_t) + ExportTableSz(info->exportTbl);

  /* allocate the heap object for the blasted representation, and initialize
   * the writer.
   */
    blastedObj = AllocBlastData (msp, totSzB);
    wr = WR_OpenMem (PTR_MLtoC(Byte_t, blastedObj), totSzB);

  /* initialize the arena headers */
    arenaHdrSz = numArenas * sizeof(heap_arena_hdr_t);
    arenaHdrsBuf = (heap_arena_hdr_t *) MALLOC (arenaHdrSz);
    for (p = arenaHdrsBuf, i = 0;  i < NUM_ARENAS;  i++) {
	if (totArenaSzB[i] > 0) {
	    p->gen		    = 0;
	    p->objKind		    = i;
	    p->info.o.baseAddr	    = 0;   /* not used */
	    p->info.o.sizeB	    = totArenaSzB[i];
	    p->info.o.roundedSzB    = -1;  /* not used */
	    p->offset		    = -1;  /* not used */
	    arenaHdrs[i]	    = p;
	    p++;
	}
	else
	    arenaHdrs[i] = NIL(heap_arena_hdr_t *);
    }
    /** WHAT ABOUT BIG OBJECTS **/

  /* blast out the image header */
    if (HeapIO_WriteImageHeader (wr, BLAST_IMAGE) == FAILURE) {
	FREE (arenaHdrsBuf);
	return BLAST_ERROR;
    }

  /* blast out the blast header */
    {
	ml_blast_hdr_t	hdr;

	hdr.numArenas = numArenas;
	hdr.numBOKinds = 0; /** FIX THIS **/
	hdr.numBORegions = 0;   /** FIX THIS **/

	if (isEXTERNTAG(obj)) {
	    ASSERT(numArenas == 0);
	    hdr.rootObj = obj;
	}
	else {
	    aid_t	aid = ADDR_TO_PAGEID(BIBOP, obj);

	    if (IS_BIGOBJ_AID(aid)) {
		embobj_info_t	*p = FindEmbObj(info->embobjTbl, obj);

		if ((p == NIL(embobj_info_t *)) || (p->kind == USED_CODE)) {
		    Error ("blasting big objects not implemented\n");
		    FREE (arenaHdrsBuf);
		    return BLAST_ERROR;
		}
		else
		    hdr.rootObj = p->relAddr;
	    }
	    else {
		Addr_t	addr = PTR_MLtoADDR(obj);
		int	gen = EXTRACT_GEN(aid) - 1;
		int	kind = EXTRACT_OBJC(aid) - 1;
		addr -= adjust[gen][kind].base;
		addr += adjust[gen][kind].offset;
		hdr.rootObj = HIO_TAG_PTR(kind, addr);
	    }
	}

	WR_Write(wr, &hdr, sizeof(hdr));
	if (WR_Error(wr)) {
	    FREE (arenaHdrsBuf);
	    return BLAST_ERROR;
	}
    }

  /* blast out the externals table */
    if (HeapIO_WriteExterns(wr, info->exportTbl) == -1) {
	FREE (arenaHdrsBuf);
	return BLAST_ERROR;
    }

  /* blast out the arena headers */
    WR_Write (wr, arenaHdrsBuf, arenaHdrSz);
    if (WR_Error(wr)) {
	FREE (arenaHdrsBuf);
	return BLAST_ERROR;
    }

  /* blast out the heap itself */
    for (i = 0;  i < NUM_ARENAS;  i++) {
	if (i == STRING_INDX) {
	  /* blast out the embedded literals */
	    BlastGC_BlastLits (wr);
	  /* blast out the rest of the strings */
	    for (j = 0;  j < maxGen;  j++) {
		arena_t	*ap = heap->gen[j]->arena[i];
		if (isACTIVE(ap)) {
		    WR_Write(wr, ap->tospBase,
			(Addr_t)(ap->nextw)-(Addr_t)(ap->tospBase));
		}
	    } /* end for */
	}
	else {
	    for (j = 0;  j < maxGen;  j++) {
		arena_t	*ap = heap->gen[j]->arena[i];
		ml_val_t	*p, *top;
		if (isACTIVE(ap)) {
		    for (p = ap->tospBase, top = ap->nextw;  p < top;  p++) {
			ml_val_t	w = *p;
			if (isBOXED(w)) {
			    aid_t		aid = ADDR_TO_PAGEID(BIBOP, w);
			    if (isUNMAPPED(aid)) {
				w = ExportCSymbol(info->exportTbl, w);
				ASSERT (w != ML_unit);
			    }
			    else if (IS_BIGOBJ_AID(aid)) {
				embobj_info_t	*objInfo
						    = FindEmbObj(info->embobjTbl, w);

				if ((objInfo == NIL(embobj_info_t *))
				|| (objInfo->kind == USED_CODE))
				    Die("blast bigobj unimplemented");
				else
				    w = objInfo->relAddr;
			    }
			    else {
			      /* adjust the pointer */
				int		gen = EXTRACT_GEN(aid)-1;
				int		kind = EXTRACT_OBJC(aid)-1;
				Addr_t	addr = PTR_MLtoADDR(w);
				addr -= adjust[gen][kind].base;
				addr += adjust[gen][kind].offset;
				w = HIO_TAG_PTR(kind, addr);
			    }
			}
			WR_Put(wr, (Word_t)w);
		    }
		}
	    } /* end for */
	}
    }

    FREE (arenaHdrsBuf);

    if (WR_Error(wr))
	return BLAST_ERROR;
    else {
	SEQHDR_ALLOC (msp, blastedObj, DESC_string, blastedObj, totSzB);
	return blastedObj;
    }

} /* end of BlastHeap */


/* AllocBlastData:
 *
 * Allocate some heap memory for blasting an object.
 */
PVT ml_val_t AllocBlastData (ml_state_t *msp, Addr_t sizeB)
{
    heap_t	    *heap = msp->ml_heap;
    int		    nWords = BYTES_TO_WORDS(sizeB);
    ml_val_t	    desc = MAKE_DESC(nWords, DTAG_raw32);
    ml_val_t	    res;

/** we probably should allocate space in the big-object region for these objects **/
    if (sizeB < heap->allocSzB-(8*ONE_K)) {
	ML_AllocWrite (msp, 0, desc);
	res = ML_Alloc (msp, nWords);
	return res;
    }
    else {
	Die ("blasting out of %d bytes not supported yet!  Increase allocation arena size.", sizeB);
    }

} /* end of AllocBlastData */

