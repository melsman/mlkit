/* export-heap.c
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 *
 * Routines to export an ML heap image.  The basic layout of the heap image is:
 *
 *   Header (Image header + Heap header)
 *   External reference table
 *   ML state info
 *   ML Heap:
 *     Big-object region descriptors
 *     Generation descriptors
 *     Heap image
 *
 *
 * Note that this will change once multiple VProcs are supported.
 */

#include "ml-osdep.h"
#include "ml-base.h"
#include "ml-limits.h"
#include "memory.h"
#include "ml-state.h"
#include "ml-objects.h"
#include "ml-globals.h"
#include "ml-heap-image.h"
#include "heap.h"
#include "c-globals-tbl.h"
#include "writer.h"
#include "heap-io.h"
#include "heap-output.h"

#define isEXTERN(bibop, w)	(isBOXED(w) && (ADDR_TO_PAGEID(bibop, w) == AID_UNMAPPED))
#define isEXTERNTAG(w)		(isDESC(w) && (GET_TAG(w) == DTAG_extern))

/* local routines */
PVT status_t ExportImage (ml_state_t *msp, int kind, FILE *file);
PVT export_table_t *ScanHeap (heap_t *heap);
PVT status_t WriteHeap (writer_t *wr, heap_t *heap);
PVT void RepairHeap (export_table_t *tbl, heap_t *heap);


/* ExportHeapImage:
 */
status_t ExportHeapImage (ml_state_t *msp, FILE *file)
{
    return ExportImage (msp, EXPORT_HEAP_IMAGE, file);

} /* end of ExportHeapImage. */


/* ExportFnImage:
 */
status_t ExportFnImage (ml_state_t *msp, ml_val_t funct, FILE *file)
{
  /* zero-out the saved parts of the ML state, and use the standard argument
   * register to hold the exported function closure.
   */
    msp->ml_arg			= funct;
    msp->ml_cont		= ML_unit;
    msp->ml_closure		= ML_unit;
    msp->ml_linkReg		= ML_unit;
    msp->ml_exnCont		= ML_unit;
    msp->ml_varReg		= ML_unit;	/* ??? */
    msp->ml_calleeSave[0]	= ML_unit;
    msp->ml_calleeSave[1]	= ML_unit;
    msp->ml_calleeSave[2]	= ML_unit;

    return ExportImage (msp, EXPORT_FN_IMAGE, file);

} /* end of ExportFnImage */


/* ExportImage:
 */
PVT status_t ExportImage (ml_state_t *msp, int kind, FILE *file)
{
    heap_t	*heap = msp->ml_heap;
    /*
    gen_t	*oldestGen = heap->gen[heap->numGens-1];
    */
    status_t	status = SUCCESS;
    export_table_t *exportTbl;
    writer_t	*wr;

#define SAVE_REG(dst, src)	{				\
	    ml_val_t	__src = (src);				\
	    if (isEXTERN(BIBOP, __src))				\
		__src = ExportCSymbol(exportTbl, __src);	\
	    (dst) = __src;					\
	}

    if ((wr = WR_OpenFile(file)) == NIL(writer_t *))
	return FAILURE;

  /* Shed any and all garbage. */
    InvokeGC (msp, 0);  /* minor collection */
    InvokeGC (msp, MAX_NGENS);

    exportTbl = ScanHeap(heap);

    {
	ml_heap_hdr_t	heapHdr;

	heapHdr.numVProcs	= 1;
	heapHdr.numGens		= heap->numGens;
	heapHdr.numArenas	= NUM_ARENAS;
	heapHdr.numBOKinds	= NUM_BIGOBJ_KINDS;
	heapHdr.numBORegions	= heap->numBORegions;
	heapHdr.cacheGen	= heap->cacheGen;
	heapHdr.allocSzB	= heap->allocSzB / MAX_NUM_PROCS;

	SAVE_REG(heapHdr.pervStruct, *PTR_MLtoC(ml_val_t, PervStruct));
	SAVE_REG(heapHdr.runTimeCompUnit, RunTimeCompUnit);
#ifdef ASM_MATH
	SAVE_REG(heapHdr.mathVec, MathVec);
#else
	heapHdr.mathVec = ML_unit;
#endif

	HeapIO_WriteImageHeader(wr, kind);
	WR_Write(wr, &heapHdr, sizeof(heapHdr));
	if (WR_Error(wr)) {
	    WR_Free(wr);
	    return FAILURE;
	}
    }

  /* export the ML state info */
    {
	ml_vproc_image_t	image;

      /* Save the live registers */
	SAVE_REG(image.sigHandler, DEREF(MLSignalHandler));
	SAVE_REG(image.stdArg, msp->ml_arg);
	SAVE_REG(image.stdCont, msp->ml_cont);
	SAVE_REG(image.stdClos, msp->ml_closure);
	SAVE_REG(image.pc, msp->ml_pc);
	SAVE_REG(image.exnCont, msp->ml_exnCont);
	SAVE_REG(image.varReg, msp->ml_varReg);
	SAVE_REG(image.calleeSave[0], msp->ml_calleeSave[0]);
	SAVE_REG(image.calleeSave[1], msp->ml_calleeSave[1]);
	SAVE_REG(image.calleeSave[2], msp->ml_calleeSave[2]);

	if (HeapIO_WriteExterns(wr, exportTbl) == FAILURE) {
	    status = FAILURE;
	    goto done;
	}

	WR_Write(wr, &image, sizeof(image));
	if (WR_Error(wr)) {
	    status = FAILURE;
	    goto done;
	}
    }

  /* Write out the heap image */
    if (WriteHeap(wr, heap) == FAILURE)
	status = FAILURE;

  done:;
    if (kind != EXPORT_FN_IMAGE)
	RepairHeap (exportTbl, heap);

    WR_Free(wr);

    return status;

} /* end of ExportImage. */


/* ScanHeap:
 *
 * Scan the heap looking for exported symbols and return an export table.
 */
PVT export_table_t *ScanHeap (heap_t *heap)
{
    export_table_t	*tbl = NewExportTbl();
    bibop_t		bibop = BIBOP;
    int			i;

  /* Scan the record, pair and array regions for references to external symbols */
    for (i = 0;  i < heap->numGens;  i++) {
#define PatchArena(indx)	{				\
	arena_t		*__ap = heap->gen[i]->arena[(indx)];	\
	ml_val_t	*__p, *__q;				\
	bool_t		needsRepair = FALSE;			\
	__p = __ap->tospBase;					\
	__q = __ap->nextw;					\
	while (__p < __q) {					\
	    ml_val_t	__w = *__p;				\
	    if (isEXTERN(bibop, __w)) {				\
		*__p = ExportCSymbol(tbl, __w);			\
		needsRepair = TRUE;				\
	    }							\
	    __p++;						\
	}							\
	__ap->needsRepair = needsRepair;			\
    } /* PatchArena */

	PatchArena(RECORD_INDX);
	PatchArena(PAIR_INDX);
	PatchArena(ARRAY_INDX);
    }

    return tbl;

} /* end of ScanHeap */


/* WriteHeap:
 *
 */
PVT status_t WriteHeap (writer_t *wr, heap_t *heap)
{
    heap_arena_hdr_t	*p, *arenaHdrs;
    bigobj_desc_t	*bdp;
    int			arenaHdrsSize, pagesize;
    long		offset;
    int			i, j;

    pagesize = GETPAGESIZE();

  /* write the big-object region descriptors */
    {
	int			sz;
	bo_region_info_t	*hdr;
	bigobj_region_t		*rp;

#ifdef BO_DEBUG
SayDebug("%d bigobject regions\n", heap->numBORegions);
#endif
	sz = heap->numBORegions * sizeof(bo_region_info_t);
	hdr = (bo_region_info_t *) MALLOC (sz);
	for (rp = heap->bigRegions, i = 0;  rp != NIL(bigobj_region_t *);  rp = rp->next, i++) {
#ifdef BO_DEBUG
PrintRegionMap(rp);
#endif
	    hdr[i].baseAddr	= MEMOBJ_BASE(rp->memObj);
	    hdr[i].firstPage	= rp->firstPage;
	    hdr[i].sizeB	= MEMOBJ_SZB(rp->memObj);
	}

	WR_Write(wr, hdr, sz);
	if (WR_Error(wr)) {
	    FREE (hdr);
	    return FAILURE;
	}

	FREE(hdr);
    }

  /* initialize the arena headers. */
    arenaHdrsSize = heap->numGens * (NUM_OBJ_KINDS * sizeof(heap_arena_hdr_t));
    arenaHdrs = (heap_arena_hdr_t *) MALLOC (arenaHdrsSize);
    offset = WR_Tell(wr) + arenaHdrsSize;
    offset = ROUNDUP(offset, pagesize);
    for (p = arenaHdrs, i = 0;  i < heap->numGens;  i++) {
	for (j = 0;  j < NUM_ARENAS;  j++, p++) {
	    arena_t		*ap = heap->gen[i]->arena[j];
	    p->gen		    = i;
	    p->objKind		    = j;
	    p->info.o.baseAddr	    = (Addr_t)(ap->tospBase);
	    p->info.o.sizeB	    = (Addr_t)(ap->nextw) - p->info.o.baseAddr;
	    p->info.o.roundedSzB    = ROUNDUP(p->info.o.sizeB, pagesize);
	    p->offset		    = (Unsigned32_t)offset;
	    offset		    += p->info.o.roundedSzB;
	}
	for (j = 0;  j < NUM_BIGOBJ_KINDS;  j++, p++) {
	    int			nObjs, nBOPages;
	    bdp = heap->gen[i]->bigObjs[j];
	    for (nObjs = nBOPages = 0;  bdp != NIL(bigobj_desc_t *);  bdp = bdp->next) {
		nObjs++;
		nBOPages += (BO_ROUNDED_SZB(bdp) >> BIGOBJ_PAGE_SHIFT);
	    }
	    p->gen		    = i;
	    p->objKind		    = j;
	    p->info.bo.numBigObjs   = nObjs;
	    p->info.bo.numBOPages   = nBOPages;
	    p->offset		    = (Unsigned32_t)offset;
	    offset		    += ((nObjs * sizeof(bigobj_hdr_t))
					+ (nBOPages << BIGOBJ_PAGE_SHIFT));
	}
    }

  /* write the arena headers out */
    WR_Write(wr, arenaHdrs, arenaHdrsSize);
    if (WR_Error(wr)) {
	FREE (arenaHdrs);
	return FAILURE;
    }

  /* write out the arenas */
    for (p = arenaHdrs, i = 0;  i < heap->numGens;  i++) {
	for (j = 0;  j < NUM_ARENAS;  j++) {
	    if (GCMessages) {
		SayDebug("write %d,%d: %d bytes [%#x..%#x) @ %#x\n",
		    i+1, j, p->info.o.sizeB,
		    p->info.o.baseAddr, p->info.o.baseAddr+p->info.o.sizeB,
		    p->offset);
	    }
	    if (p->info.o.sizeB > 0) {
		WR_Seek(wr, p->offset);
		WR_Write(wr, (void *)(p->info.o.baseAddr), p->info.o.sizeB);
		if (WR_Error(wr)) {
		    FREE (arenaHdrs);
		    return FAILURE;
		}
	    }
	    p++;
	}
	for (j = 0;  j < NUM_BIGOBJ_KINDS;  j++) {
	    int			hdrSizeB;
	    bigobj_hdr_t	*hdr, *q;

	    if (p->info.bo.numBigObjs > 0) {
		hdrSizeB = p->info.bo.numBigObjs * sizeof(bigobj_hdr_t);
		hdr = (bigobj_hdr_t *) MALLOC (hdrSizeB);
		if (GCMessages) {
		    SayDebug("write %d,%d: %d big objects (%d pages) @ %#x\n",
			i+1, j, p->info.bo.numBigObjs, p->info.bo.numBOPages,
			p->offset);
		}
	      /* initialize the big-object headers */
		q = hdr;
		for (bdp = heap->gen[i]->bigObjs[j];  bdp != NIL(bigobj_desc_t *);  bdp = bdp->next) {
		    q->gen		= bdp->gen;
		    q->objKind		= j;
		    q->baseAddr	= (Addr_t)(bdp->obj);
		    q->sizeB		= bdp->sizeB;
		    q++;
		}
	      /* write the big-object headers */
		WR_Write (wr, hdr, hdrSizeB);
		if (WR_Error(wr)) {
		    FREE (hdr);
		    FREE (arenaHdrs);
		    return FAILURE;
		}
	      /* write the big-objects */
		for (bdp = heap->gen[i]->bigObjs[j];  bdp != NIL(bigobj_desc_t *);  bdp = bdp->next) {
		    WR_Write(wr, (char *)(bdp->obj), BO_ROUNDED_SZB(bdp));
		    if (WR_Error(wr)) {
			FREE (hdr);
			FREE (arenaHdrs);
			return FAILURE;
		    }
		}
		FREE (hdr);
	    }
	    p++;
	}
    }

    FREE (arenaHdrs);

    return SUCCESS;

} /* end of WriteHeap. */

/* RepairHeap:
 */
PVT void RepairHeap (export_table_t *tbl, heap_t *heap)
{
    int			i;

  /* repair the in-memory heap */
    for (i = 0;  i < heap->numGens;  i++) {
#define RepairArena(indx)	{				\
	arena_t		*__ap = heap->gen[i]->arena[(indx)];	\
	if (__ap->needsRepair) {				\
	    ml_val_t	*__p, *__q;				\
	    __p = __ap->tospBase;				\
	    __q = __ap->nextw;					\
	    while (__p < __q) {					\
		ml_val_t	__w = *__p;			\
		if (isEXTERNTAG(__w)) {				\
		    *__p = AddrOfCSymbol(tbl, __w);		\
		}						\
		__p++;						\
	    }							\
	}							\
	__ap->needsRepair = FALSE;				\
    } /* RepairArena */

	RepairArena(RECORD_INDX);
	RepairArena(PAIR_INDX);
	RepairArena(ARRAY_INDX);
    }

    FreeExportTbl (tbl);

} /* end of RepairHeap */
