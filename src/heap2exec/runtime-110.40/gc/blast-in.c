/* blast-in.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 */

#include <stdio.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "heap.h"
#include "ml-heap-image.h"
#include "c-globals-tbl.h"
#include "heap-input.h"


/* local routines */
PVT status_t ReadImage (ml_state_t *msp, inbuf_t *bp, ml_val_t *objRef);


/* BlastIn:
 *
 * Build an ML heap object from a sequence of bytes; the fd is the underlying
 * file descriptor (== -1, if blasting from a string), buf is any pre-read
 * bytes of data, and nbytesP points to the number of bytes in buf.
 */
ml_val_t BlastIn (ml_state_t *msp, Byte_t *buf, long len, bool_t *errFlg)
{
    inbuf_t		inBuf;
    ml_image_hdr_t	hdr;
    ml_val_t		obj;

    inBuf.needsSwap	= FALSE;
    inBuf.file		= NULL;
    inBuf.base		= buf;
    inBuf.buf		= buf;
    inBuf.nbytes	= len;

  /* read the object header */
    if (HeapIO_ReadBlock (&inBuf, &hdr, sizeof(hdr)) == FAILURE) {
	*errFlg = TRUE;
	return ML_unit;
    }
    if (hdr.byteOrder != ORDER) {
	if (BIGENDIAN_TO_HOST(hdr.byteOrder) != ORDER) {
	    *errFlg = TRUE;
	    return ML_unit;
	}
	hdr.magic = BIGENDIAN_TO_HOST(hdr.magic);
	hdr.kind = BIGENDIAN_TO_HOST(hdr.kind);
	inBuf.needsSwap = TRUE;
    }
    if (hdr.magic != BLAST_MAGIC) {
	*errFlg = TRUE;
	return ML_unit;
    }

    switch (hdr.kind) {
      case BLAST_IMAGE:
	if (ReadImage(msp, &inBuf, &obj) == FAILURE) {
	    *errFlg = TRUE;
	    return ML_unit;
	}
	break;
      case BLAST_UNBOXED: {
	    ml_blast_hdr_t	bhdr;
	    if (HeapIO_ReadBlock(&inBuf, &bhdr, sizeof(bhdr)) == FAILURE) {
	        *errFlg = TRUE;
	        return ML_unit;
	    }
	    else
		obj = bhdr.rootObj;
	} break;
      default:
	*errFlg = TRUE;
	return ML_unit;
    }

    return obj;

} /* end of BlastIn */


/* ReadImage:
 */
PVT status_t ReadImage (ml_state_t *msp, inbuf_t *bp, ml_val_t *objRef)
{
    ml_blast_hdr_t	blastHdr;
    ml_val_t		*externs;
    heap_arena_hdr_t	*arenaHdrs[NUM_OBJ_KINDS], *arenaHdrsBuf;
    int			arenaHdrsSize, i;
    gen_t		*gen1 = msp->ml_heap->gen[0];

    if ((HeapIO_ReadBlock(bp, &blastHdr, sizeof(blastHdr)) == FAILURE)
    || (blastHdr.numArenas > NUM_ARENAS)
    || (blastHdr.numBOKinds > NUM_BIGOBJ_KINDS))
	return FAILURE;

  /* read the externals table */
    externs = HeapIO_ReadExterns (bp);

  /* read the arena headers. */
    arenaHdrsSize = (blastHdr.numArenas + blastHdr.numBOKinds)
			* sizeof(heap_arena_hdr_t);
    arenaHdrsBuf = (heap_arena_hdr_t *) MALLOC (arenaHdrsSize);
    if (HeapIO_ReadBlock (bp, arenaHdrsBuf, arenaHdrsSize) == FAILURE) {
	FREE (arenaHdrsBuf);
	return FAILURE;
    }
    for (i = 0;  i < NUM_OBJ_KINDS;  i++)
	arenaHdrs[i] = NIL(heap_arena_hdr_t *);
    for (i = 0;  i < blastHdr.numArenas;  i++) {
	heap_arena_hdr_t	*p = &(arenaHdrsBuf[i]);
	arenaHdrs[p->objKind] = p;
    }
    /** DO BIG OBJECT HEADERS TOO **/

  /* check the heap to see if there is enough free space in the 1st generation */
    {
	Addr_t	allocSzB = msp->ml_heap->allocSzB;
	bool_t	needsGC = FALSE;

	for (i = 0;  i < NUM_ARENAS;  i++) {
	    arena_t	*ap = gen1->arena[i];
	    if ((arenaHdrs[i] != NIL(heap_arena_hdr_t *)) && ((! isACTIVE(ap))
	    || (AVAIL_SPACE(ap) < arenaHdrs[i]->info.o.sizeB + allocSzB))) {
		needsGC = TRUE;
		ap->reqSizeB = arenaHdrs[i]->info.o.sizeB;
	    }
	}
	if (needsGC) {
	    if (bp->nbytes > 0) {
	      /* the GC may cause the buffer to move */
		ml_val_t	buffer = PTR_CtoML(bp->base);
		InvokeGCWithRoots (msp, 1, &buffer, NIL(ml_val_t *));
		if (buffer != PTR_CtoML(bp->base)) {
		  /* the buffer moved, so adjust the buffer pointers */
		    Byte_t	*newBase = PTR_MLtoC(Byte_t, buffer);
		    bp->buf = newBase + (bp->buf - bp->base);
		    bp->base = newBase;
		}
	    }
	    else
		InvokeGC (msp, 1);
	}
    }

    /** Read the blasted objects **/
    {
	Addr_t	arenaBase[NUM_ARENAS];

	for (i = 0;  i < NUM_ARENAS;  i++) {
	    if (arenaHdrs[i] != NIL(heap_arena_hdr_t *)) {
	        arena_t	*ap = gen1->arena[i];
	        arenaBase[i] = (Addr_t)(ap->nextw);
	        HeapIO_ReadBlock (bp, (ap->nextw), arenaHdrs[i]->info.o.sizeB);
/*SayDebug ("[%2d] Read [%#x..%#x)\n", i+1, ap->nextw,*/
/*(Addr_t)(ap->nextw)+arenaHdrs[i]->info.o.sizeB);*/
	    }
	}

      /* adjust the pointers */
	for (i = 0;  i < NUM_ARENAS;  i++) {
	    if (arenaHdrs[i] != NIL(heap_arena_hdr_t *)) {
		arena_t	*ap = gen1->arena[i];
		if (i != STRING_INDX) {
		    ml_val_t	*p, *stop;
		    p = ap->nextw;
		    stop = (ml_val_t *)((Addr_t)p + arenaHdrs[i]->info.o.sizeB);
		    while (p < stop) {
		        ml_val_t	w = *p;
		        if (! isUNBOXED(w)) {
			    if (isEXTERNTAG(w)) {
			        w = externs[EXTERNID(w)];
			    }
			    else if (! isDESC(w)) {
/*SayDebug ("adjust (@%#x) %#x --> ", p, w);*/
			        w = PTR_CtoML(arenaBase[HIO_GET_ID(w)] + HIO_GET_OFFSET(w));
/*SayDebug ("%#x\n", w);*/
			    }
		            *p = w;
		        }
		        p++;
		    }
		    ap->nextw	=
		    ap->sweep_nextw	= stop;
	        }
	        else
		    ap->nextw = (ml_val_t *)((Addr_t)(ap->nextw)
				+ arenaHdrs[i]->info.o.sizeB);
	    }
	} /* end of for */

      /* adjust the root object pointer */
	if (isEXTERNTAG(blastHdr.rootObj))
	    *objRef = externs[EXTERNID(blastHdr.rootObj)];
	else
	    *objRef = PTR_CtoML(
		    arenaBase[HIO_GET_ID(blastHdr.rootObj)]
		    + HIO_GET_OFFSET(blastHdr.rootObj));
/*SayDebug ("root = %#x, adjusted = %#x\n", blastHdr.rootObj, *objRef);*/
    }

    FREE (arenaHdrsBuf);
    FREE (externs);

    return SUCCESS;

} /* end of ReadImage */
