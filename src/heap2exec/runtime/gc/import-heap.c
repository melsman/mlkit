/* import-heap.c
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 *
 * Routines to import an ML heap image.
 */

#include <stdio.h>
#include <string.h>
#include "ml-base.h"
#include "machine-id.h"
#include "memory.h"
#include "reg-mask.h"
#include "cache-flush.h"
#include "ml-state.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-globals.h"
#include "card-map.h"
#include "heap.h"
#include "ml-heap-image.h"
#include "c-globals-tbl.h"
#include "addr-hash.h"
#include "heap-input.h"
#include "heap-io.h"

#ifndef SEEK_SET
#  define SEEK_SET	0
#  define SEEK_END	2
#endif

#ifdef DEBUG
PVT void PrintRegionMap (bo_region_reloc_t *r)
{
    bo_reloc_t		*dp, *dq;
    int			i;

    SayDebug ("region @%#x: |", r->firstPage);
    for (i = 0, dq = r->objMap[0];  i < r->nPages;  i++) {
	dp = r->objMap[i];
	if (dp != dq) {
	    SayDebug ("|");
	    dq = dp;
	}
	if (dp == NIL(bo_reloc_t *))
	    SayDebug ("_");
	else
	    SayDebug ("X");
    }
    SayDebug ("|\n");

} /* end of PrintRegionMap */
#endif


/* local routines */
PVT void ReadHeap (inbuf_t *bp, ml_heap_hdr_t *hdr, ml_state_t *msp, ml_val_t *externs);
PVT bigobj_desc_t *AllocBODesc (bigobj_desc_t *, bigobj_hdr_t *, bo_region_reloc_t *);
PVT void RepairHeap (
	heap_t *, aid_t *, Addr_t [MAX_NUM_GENS][NUM_ARENAS],
	addr_tbl_t *, ml_val_t *);
PVT ml_val_t RepairWord (
	ml_val_t w, aid_t *oldBIBOP, Addr_t addrOffset[MAX_NUM_GENS][NUM_ARENAS],
	addr_tbl_t *boRegionTbl, ml_val_t *externs);
PVT int RepairBORef (aid_t *bibop, aid_t id, ml_val_t *ref, ml_val_t oldObj);
PVT bo_reloc_t *AddrToRelocInfo (aid_t *, addr_tbl_t *, aid_t, Addr_t);

#define READ(bp,obj)	HeapIO_ReadBlock(bp, &(obj), sizeof(obj))

#ifdef HACKED_STANDALONE
PVT long	heapStart = -1;
#endif


/* ImportHeapImage:
 */
ml_state_t *ImportHeapImage (const char *fname, heap_params_t *params)
{
    int			i;
    ml_state_t		*msp;
    ml_image_hdr_t	imHdr;
    ml_heap_hdr_t	heapHdr;
    ml_val_t		*externs;
    ml_vproc_image_t	image;
    inbuf_t		inBuf;

  /* Resolve the name of the image.  If the file exists use it, otherwise try the
   * pathname with the machine ID as an extension.
   */
    if ((inBuf.file = fopen(fname, "rb")) != NULL) {
	if (! SilentLoad)
	    Say("loading %s ", fname);
    }
    else {
	char	buf[1024];

	if (QualifyImageName(strcpy(buf, fname))
	&& ((inBuf.file = fopen(buf, "rb")) != NULL)) {
	    if (! SilentLoad)
		Say("loading %s ", buf);
	}
	else
	    Die ("unable to open heap image \"%s\"\n", fname);
    }


    inBuf.needsSwap = FALSE;
    inBuf.buf	    = NIL(Byte_t *);
    inBuf.nbytes    = 0;

#ifdef HACKED_STANDALONE
  /* note that the seek may (will) succeed even if there's nothing there */
  /* for now, we'll be content to just fail on the reads below */
    if (StandAlone) {
	if ((fseek (inBuf.file, -sizeof(long), SEEK_END) != 0)
	||  (fread(&heapStart, sizeof(long), 1, inBuf.file) != 1)
	||  (fseek (inBuf.file, heapStart, SEEK_SET) != 0)) {
	    Die ("unable to seek to internal image in stand-alone at %x\n",
		heapStart);
	}
    }
#endif

    READ(&inBuf, imHdr);
    if (imHdr.byteOrder != ORDER)
	Die ("incorrect byte order in heap image\n");
    if (imHdr.magic != IMAGE_MAGIC)
	Die ("bad magic number (%#x) in heap image\n", imHdr.magic);
    if ((imHdr.kind != EXPORT_HEAP_IMAGE) && (imHdr.kind != EXPORT_FN_IMAGE))
	Die ("bad image kind (%d) in heap image\n", imHdr.kind);
    READ(&inBuf, heapHdr);

  /* check for command-line overrides of heap parameters. */
    if (params->allocSz == 0) params->allocSz = heapHdr.allocSzB;
    if (params->numGens < heapHdr.numGens) params->numGens = heapHdr.numGens;
    if (params->cacheGen < 0) params->cacheGen = heapHdr.cacheGen;

    msp = AllocMLState (FALSE, params);

  /* get the run-time pointers into the heap */
    *PTR_MLtoC(ml_val_t, PervStruct) = heapHdr.pervStruct;
    RunTimeCompUnit = heapHdr.runTimeCompUnit;
#ifdef ASM_MATH
    MathVec = heapHdr.mathVec;
#endif

  /* read the externals table */
    externs = HeapIO_ReadExterns (&inBuf);

  /* read and initialize the ML state info */
    READ(&inBuf, image);
    if (imHdr.kind == EXPORT_HEAP_IMAGE) {
      /* Load the live registers */
	ASSIGN(MLSignalHandler, image.sigHandler);
	msp->ml_liveRegMask	= RET_MASK;
	msp->ml_arg		= image.stdArg;
	msp->ml_closure		= image.stdClos;
	msp->ml_cont		= image.stdCont;
	msp->ml_exnCont		= image.exnCont;
	msp->ml_pc		= image.pc;
#if (CALLEESAVE > 0)
	for (i = 0;  i < CALLEESAVE;  i++)
	    msp->ml_calleeSave(i) = image.calleeSaves[i];
#endif
#if (FLOAT_CALLEESAVE > 0)
	/** LOAD FLOAT CALLEE SAVES **/
#endif
      /* read the ML heap */
	ReadHeap (&inBuf, &heapHdr, msp, externs);
      /* GC message are on by default for interactive images */
	GCMessages = TRUE;
    }
    else {  /* EXPORT_FN_IMAGE */
	ml_val_t	funct, cmdName, args;
      /* restore the signal handler */
	ASSIGN(MLSignalHandler, image.sigHandler);
      /* read the ML heap */
	msp->ml_arg		= image.stdArg;
	ReadHeap (&inBuf, &heapHdr, msp, externs);
      /* initialize the calling context (taken from ApplyMLFn) */
	funct			= msp->ml_arg;
	msp->ml_exnCont		= PTR_CtoML(handle_v+1);
	msp->ml_varReg		= ML_unit;
	msp->ml_cont		= PTR_CtoML(return_c);
	msp->ml_closure		= funct;
	msp->ml_pc		=
	msp->ml_linkReg		= GET_CODE_ADDR(funct);
      /* setup the arguments to the imported function */
	cmdName = ML_CString(msp, MLCmdName);
	args = ML_CStringList (msp, CmdLineArgs);
	REC_ALLOC2(msp, msp->ml_arg, cmdName, args);
/*
SayDebug("arg = %#x : [%#x, %#x]\n", msp->ml_arg, REC_SEL(msp->ml_arg, 0), REC_SEL(msp->ml_arg, 1));
*/
      /* GC message are off by default for exportFn images */
	GCMessages = FALSE;
    }

    FREE (externs);
    fclose (inBuf.file);

    if (! SilentLoad)
	Say(" done\n");

    return msp;

} /* end of ImportHeapImage */

/* ReadHeap:
 */
PVT void ReadHeap (inbuf_t *bp, ml_heap_hdr_t *hdr, ml_state_t *msp, ml_val_t *externs)
{
    heap_t		*heap = msp->ml_heap;
    heap_arena_hdr_t	*arenaHdrs, *p, *q;
    int			arenaHdrsSize;
    int			i, j, k;
    long		prevSzB[NUM_ARENAS], sz;
    bibop_t		oldBIBOP;
    Addr_t		addrOffset[MAX_NUM_GENS][NUM_ARENAS];
    bo_region_reloc_t	*boRelocInfo;
    addr_tbl_t		*boRegionTbl;

  /* Allocate a BIBOP for the imported heap image's address space. */
#ifdef TWO_LEVEL_MAP
#  error two level map not supported
#else
    oldBIBOP = NEW_VEC (aid_t, BIBOP_SZ);
#endif

  /* read in the big-object region descriptors for the old address space */
    {
	int			sz;
	bo_region_info_t	*boRgnHdr;
	bigobj_region_t		*rp;

	boRegionTbl = MakeAddrTbl(BIBOP_SHIFT+1, hdr->numBORegions);
	sz = hdr->numBORegions * sizeof(bo_region_info_t);
	boRgnHdr = (bo_region_info_t *) MALLOC (sz);
	HeapIO_ReadBlock (bp, boRgnHdr, sz);

	boRelocInfo = NEW_VEC(bo_region_reloc_t, hdr->numBORegions);
	for (i = 0;  i < hdr->numBORegions;  i++) {
	    MarkRegion(oldBIBOP,
		(ml_val_t *)(boRgnHdr[i].baseAddr),
		RND_MEMOBJ_SZB(boRgnHdr[i].sizeB),
		AID_BIGOBJ(1));
	    oldBIBOP[BIBOP_ADDR_TO_INDEX(boRgnHdr[i].baseAddr)] = AID_BIGOBJ_HDR(MAX_NUM_GENS);
	    boRelocInfo[i].firstPage = boRgnHdr[i].firstPage;
	    boRelocInfo[i].nPages =
		(boRgnHdr[i].sizeB - (boRgnHdr[i].firstPage - boRgnHdr[i].baseAddr))
		    >> BIGOBJ_PAGE_SHIFT;
	    boRelocInfo[i].objMap = NEW_VEC(bo_reloc_t *, boRelocInfo[i].nPages);
	    for (j = 0;  j < boRelocInfo[i].nPages;  j++)
		boRelocInfo[i].objMap[j] = NIL(bo_reloc_t *);
	    AddrTblInsert (boRegionTbl, boRgnHdr[i].baseAddr, &(boRelocInfo[i]));
	}
	FREE (boRgnHdr);
    }

  /* read the arena headers. */
    arenaHdrsSize = hdr->numGens * NUM_OBJ_KINDS * sizeof(heap_arena_hdr_t);
    arenaHdrs = (heap_arena_hdr_t *) MALLOC (arenaHdrsSize);
    HeapIO_ReadBlock (bp, arenaHdrs, arenaHdrsSize);

    for (i = 0;  i < NUM_ARENAS;  i++)
	prevSzB[i] = heap->allocSzB;

  /* allocate the arenas and read in the heap image. */
    for (p = arenaHdrs, i = 0;  i < hdr->numGens;  i++) {
	gen_t	*gen = heap->gen[i];

      /* compute the space required for this generation, and mark the oldBIBOP
       * to reflect the old address space.
       */
	for (q = p, j = 0;  j < NUM_ARENAS;  j++) {
	    MarkRegion (oldBIBOP,
		(ml_val_t *)(q->info.o.baseAddr),
		RND_MEMOBJ_SZB(q->info.o.sizeB),
		gen->arena[j]->id);
	    sz = q->info.o.sizeB + prevSzB[j];
	    if ((j == PAIR_INDX) && (sz > 0))
		sz += 2*WORD_SZB;
	    gen->arena[j]->tospSizeB = RND_MEMOBJ_SZB(sz);
	    prevSzB[j] = q->info.o.sizeB;
	    q++;
	}

      /* Allocate space for the generation */
	if (NewGeneration(gen) == FAILURE)
	    Die ("unable to allocated space for generation %d\n", i+1);
	if (isACTIVE(gen->arena[ARRAY_INDX]))
	    NewDirtyVector (gen);

      /* read in the arenas for this generation and initialize the
       * address offset table.
       */
	for (j = 0;  j < NUM_ARENAS;  j++) {
	    arena_t		*ap = gen->arena[j];
	    long		offset;

	    if (p->info.o.sizeB > 0) {
		addrOffset[i][j] = (Addr_t)(ap->tospBase) - (Addr_t)(p->info.o.baseAddr);
#ifdef HACKED_STANDALONE
	      /* assuming here that ReadHeap is only called from ImportHeapImage
	       * and that ImportHeapImage is only called from LoadML on startup.
               * Otherwise, StandAlone needs to be set to FALSE after the intial
	       * load; or need extra offset param here, etc.
	       */
		offset = (long)(p->offset) + (StandAlone ? heapStart : 0);
#else
		offset = (long)(p->offset);
#endif
		if (fseek (bp->file, offset, SEEK_SET) != 0)
		    Die("unable to seek on heap image\n");
		HeapIO_ReadBlock(bp, (ap->tospBase), p->info.o.sizeB);
		ap->nextw	= (ml_val_t *)((Addr_t)(ap->tospBase) + p->info.o.sizeB);
		ap->oldTop	= ap->tospBase;
	    }
	    else if (isACTIVE(ap)) {
		ap->oldTop = ap->tospBase;
	    }
	    if (! SilentLoad) {
		Say(".");
	    }
	    p++;
	}
      /* read in the big-object arenas */
	for (j = 0;  j < NUM_BIGOBJ_KINDS;  j++) {
	    Addr_t		totSizeB;
	    bigobj_desc_t	*freeObj, *bdp;
	    bigobj_region_t	*freeRegion;
	    bigobj_hdr_t	*boHdrs;
	    int			boHdrSizeB, indx;
	    bo_region_reloc_t   *region;

	    if (p->info.bo.numBOPages > 0) {
		totSizeB = p->info.bo.numBOPages << BIGOBJ_PAGE_SHIFT;
		freeObj = BO_AllocRegion (heap, totSizeB);
		freeRegion = freeObj->region;
		freeRegion->minGen = i;
		MarkRegion (BIBOP, (ml_val_t *)freeRegion,
		    MEMOBJ_SZB(freeRegion->memObj), AID_BIGOBJ(i));
		BIBOP[BIBOP_ADDR_TO_INDEX(freeRegion)] = AID_BIGOBJ_HDR(i);

	      /* read in the big-object headers */
		boHdrSizeB = p->info.bo.numBigObjs * sizeof(bigobj_hdr_t);
		boHdrs = (bigobj_hdr_t *) MALLOC (boHdrSizeB);
		HeapIO_ReadBlock (bp, boHdrs, boHdrSizeB);

	      /* read in the big-objects */
		HeapIO_ReadBlock (bp, (void *)(freeObj->obj), totSizeB);
		if (j == CODE_INDX) {
		    FlushICache ((void *)(freeObj->obj), totSizeB);
		}

	      /* setup the big-object descriptors and per-object relocation info */
		for (k = 0;  k < p->info.bo.numBigObjs;  k++) {
		  /* find the region relocation info for the object's region in
		   * the exported heap.
		   */
		    for (indx = BIBOP_ADDR_TO_INDEX(boHdrs[k].baseAddr);
			!BO_IS_HDR(oldBIBOP[indx]);
			indx--)
			continue;
		    region = LookupBORegion (boRegionTbl, indx);
		  /* allocate the big-object descriptor for the object, and
		   * link it into the list of big-objects for its generation.
		   */
		    bdp = AllocBODesc (freeObj, &(boHdrs[k]), region);
		    bdp->next = gen->bigObjs[j];
		    gen->bigObjs[j] = bdp;
		    ASSERT(bdp->gen == i+1);

		    if (DumpObjectStrings && (j == CODE_INDX)) {
		      /* dump the comment string of the code object */
			char		buf[256];
			if (BO_GetCodeObjTag(bdp, buf, sizeof(buf)) != NIL(char *))
			    SayDebug ("[%6d bytes] %s\n", bdp->sizeB, buf);
		    }
		}

		if (freeObj != bdp) {
		  /* there was some extra space left in the region */
		    ADD_BODESC(heap->freeBigObjs, freeObj);
		}

		FREE (boHdrs);
	    }
	    if (! SilentLoad) {
		Say(".");
	    }
	    p++;
	}
    }

    RepairHeap (heap, oldBIBOP, addrOffset, boRegionTbl, externs);

  /* Adjust the run-time globals that point into the heap */
    *PTR_MLtoC(ml_val_t, PervStruct) = RepairWord (
	*PTR_MLtoC(ml_val_t, PervStruct),
	oldBIBOP, addrOffset, boRegionTbl, externs);
    RunTimeCompUnit = RepairWord (
	RunTimeCompUnit, oldBIBOP, addrOffset, boRegionTbl, externs);
#ifdef ASM_MATH
    MathVec = RepairWord (MathVec, oldBIBOP, addrOffset, boRegionTbl, externs);
#endif

  /* Adjust the ML registers to the new address space */
    ASSIGN(MLSignalHandler, RepairWord (
	DEREF(MLSignalHandler), oldBIBOP, addrOffset, boRegionTbl, externs));
    for (i = 0;  i < NROOTS;  i++) {
	msp->ml_roots[i] = RepairWord (
	    msp->ml_roots[i], oldBIBOP, addrOffset, boRegionTbl, externs);
    }

  /* release storage */
    for (i = 0; i < hdr->numBORegions;  i++) {
	bo_reloc_t	*p;
	for (p = NIL(bo_reloc_t *), j = 0;  j < boRelocInfo[i].nPages;  j++) {
	    if ((boRelocInfo[i].objMap[j] != NIL(bo_reloc_t *))
	    && (boRelocInfo[i].objMap[j] != p)) {
		FREE (boRelocInfo[i].objMap[j]);
		p = boRelocInfo[i].objMap[j];
	    }
	}
    }
    FreeAddrTbl (boRegionTbl, FALSE);
    FREE (boRelocInfo);
    FREE (arenaHdrs);
    FREE (oldBIBOP);

  /* reset the sweep_nextw pointers */
    for (i = 0;  i < heap->numGens;  i++) {
	gen_t	*gen = heap->gen[i];
	for (j = 0;  j < NUM_ARENAS;  j++) {
	    arena_t		*ap = gen->arena[j];
	    if (isACTIVE(ap))
		ap->sweep_nextw = ap->nextw;
	}
    }

} /* end of ReadHeap. */

/* AllocBODesc:
 *
 */
PVT bigobj_desc_t *AllocBODesc (
    bigobj_desc_t   *free,
    bigobj_hdr_t    *objHdr,
    bo_region_reloc_t *oldRegion)
{
    bigobj_region_t *region;
    bigobj_desc_t   *newObj;
    bo_reloc_t	    *relocInfo;
    int		    i, totSzB, firstPage, npages;

    totSzB = ROUNDUP(objHdr->sizeB, BIGOBJ_PAGE_SZB);
    npages = (totSzB >> BIGOBJ_PAGE_SHIFT);
    region = free->region;
    if (free->sizeB == totSzB)
      /* allocate the whole free area to the object */
	newObj = free;
    else {
      /* split the free object */
	newObj		= NEW_OBJ(bigobj_desc_t);
	newObj->obj	= free->obj;
	newObj->region	= region;
	free->obj	= (Addr_t)(free->obj) + totSzB;
	free->sizeB	-= totSzB;
	firstPage	= ADDR_TO_BOPAGE(region, newObj->obj);
	for (i = 0;  i < npages;  i++)
	    region->objMap[firstPage+i] = newObj;
    }

    newObj->sizeB	= objHdr->sizeB;
    newObj->state	= BO_YOUNG;
    newObj->gen		= objHdr->gen;
    newObj->objc	= objHdr->objKind;
    region->nFree	-= npages;

  /* setup the relocation info */
    relocInfo = NEW_OBJ(bo_reloc_t);
    relocInfo->oldAddr = objHdr->baseAddr;
    relocInfo->newObj = newObj;
    firstPage = ADDR_TO_BOPAGE(oldRegion, objHdr->baseAddr);
    for (i = 0;  i < npages;  i++)
	oldRegion->objMap[firstPage+i] = relocInfo;

    return newObj;

} /* end of AllocBODesc */

/* RepairHeap:
 *
 * Scan the heap, replacing external references with their addresses and
 * adjusting pointers.
 */
PVT void RepairHeap (
    heap_t *heap,
    aid_t *oldBIBOP,
    Addr_t addrOffset[MAX_NUM_GENS][NUM_ARENAS],
    addr_tbl_t *boRegionTbl,
    ml_val_t *externs)
{
    int		i;

    for (i = 0;  i < heap->numGens;  i++) {
	gen_t	*gen = heap->gen[i];
#ifndef BIT_CARDS
#define MARK(cm, p, g)	MARK_CARD(cm, p, g)
#else
#define MARK(cm, p, g)	MARK_CARD(cm, p)
#endif
#define RepairArena(indx)	{						\
	    arena_t		*__ap = gen->arena[(indx)];			\
	    ml_val_t	*__p, *__q;						\
	    __p = __ap->tospBase;						\
	    __q = __ap->nextw;							\
	    while (__p < __q) {							\
		ml_val_t	__w = *__p;					\
		int		__gg, __objc;					\
		if (isBOXED(__w)) {						\
		    Addr_t	__obj = PTR_MLtoADDR(__w);			\
		    aid_t	__aid = ADDR_TO_PAGEID(oldBIBOP, __obj);	\
		    if (IS_BIGOBJ_AID(__aid)) {					\
			bo_reloc_t	*__dp;					\
			__dp = AddrToRelocInfo (oldBIBOP, boRegionTbl,		\
				__aid, __obj);					\
			*__p = PTR_CtoML((__obj - __dp->oldAddr) 		\
				+ __dp->newObj->obj);				\
			__gg = __dp->newObj->gen-1;				\
		    }								\
		    else {							\
			__gg = EXTRACT_GEN(__aid)-1;				\
			__objc = EXTRACT_OBJC(__aid)-1;				\
			*__p = PTR_CtoML(__obj + addrOffset[__gg][__objc]);	\
		    }								\
		    if (((indx) == ARRAY_INDX) && (__gg < i)) {			\
			MARK(gen->dirty, __p, __gg+1);	/** **/			\
		    }								\
		}								\
		else if (isEXTERNTAG(__w)) {					\
		    *__p = externs[EXTERNID(__w)];				\
		}								\
		__p++;								\
	    }									\
	} /* RepairArena */

	RepairArena(RECORD_INDX);
	RepairArena(PAIR_INDX);
	RepairArena(ARRAY_INDX);
    }

} /* end of RepairHeap */

/* RepairWord:
 */
PVT ml_val_t RepairWord (
    ml_val_t w,
    aid_t *oldBIBOP,
    Addr_t addrOffset[MAX_NUM_GENS][NUM_ARENAS],
    addr_tbl_t *boRegionTbl,
    ml_val_t *externs)
{
    if (isBOXED(w)) {
	Addr_t	obj = PTR_MLtoADDR(w);
	aid_t	aid = ADDR_TO_PAGEID(oldBIBOP, obj);
	if (IS_BIGOBJ_AID(aid)) {
	    bo_reloc_t	*dp;
	    dp = AddrToRelocInfo (oldBIBOP, boRegionTbl, aid, obj);
	    return PTR_CtoML((obj - dp->oldAddr) + dp->newObj->obj);
	}
	else {
	    int	g = EXTRACT_GEN(aid)-1;
	    int	objc = EXTRACT_OBJC(aid)-1;
	    return PTR_CtoML(PTR_MLtoC(char, w) + addrOffset[g][objc]);
	}
    }
    else if (isEXTERNTAG(w)) {
	return externs[EXTERNID(w)];
    }
    else
	return w;

} /* end of RepairWord */


/* AddrToRelocInfo:
 */
PVT bo_reloc_t *AddrToRelocInfo (
    aid_t *oldBIBOP, 
    addr_tbl_t *boRegionTbl, 
    aid_t id, 
    Addr_t oldObj)
{
    int		    indx;
    bo_region_reloc_t *region;

    for (indx = BIBOP_ADDR_TO_INDEX(oldObj);  !BO_IS_HDR(id);  id = oldBIBOP[--indx])
	continue;

  /* find the old region descriptor */
    region = LookupBORegion (boRegionTbl, indx);

    if (region == NIL(bo_region_reloc_t *))
	Die ("unable to map big-object @ %#x; index = %#x, id = %#x\n",
	    oldObj, indx, (unsigned)id);

    return ADDR_TO_BODESC(region, oldObj);

} /* end of AddrToRelocInfo */
