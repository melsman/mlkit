/* ml-heap-image.h
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 *
 * The definitions and typedefs that describe the layout of an ML
 * heap image in a file.  This can be either an exported heap, or
 * a blasted object.
 *
 * These files have the following basic layout:
 *
 *  Image header
 *  Heap/Blast header
 *  External reference table
 *  Image
 *
 * where the format of Image depends on the kind (heap vs. blast).
 */

#ifndef _ML_IMAGE_
#define _ML_IMAGE_

#ifndef _ML_SIZES_
#include "ml-sizes.h"
#endif

#ifndef _ML_STATE_
#include "ml-state.h"
#endif

#ifndef _HEAP_
#include "heap.h"
#endif

/* tag to identify image byte order */
#define ORDER		0x00112233

/* heap image version identifier (date in 00mmddyy form) */
#define IMAGE_MAGIC	0x00022499

/* blasted heap image version identifier (date in 00mmddyy form) */
#define BLAST_MAGIC	0x00070995

/* the kind of heap image */
#define EXPORT_HEAP_IMAGE	1
#define EXPORT_FN_IMAGE		2
#define BLAST_IMAGE		3
#define BLAST_UNBOXED		4	/* a blasted unboxed value */

typedef struct {	    /* The magic number, and other version info */
    Unsigned32_t    byteOrder;	/* ORDER tag */
    Unsigned32_t    magic;	/* magic number */
    Unsigned32_t    kind;	/* EXPORT_HEAP_IMAGE, etc. */
    char	    arch[12];	/* the exporting machine's architecture */
    char	    opsys[12];	/* the exporting machine's operating system */
} ml_image_hdr_t;


typedef struct {	    /* The header for a heap image */
    int		numVProcs;	/* The number of virtual processors */
    int		numGens;	/* The number of heap generations */
    int		numArenas;	/* The number of small-object arenas (one per kind) */
    int		numBOKinds;	/* The number of big-object kinds */
    int		numBORegions;	/* The number of big-object regions in the */
				/* exporting address space. */
    int		cacheGen;	/* The oldest cached generation */    
    Addr_t	allocSzB;	/* The size of the allocation arena */
			    /* heap objects that are referred to by the runtime */
    ml_val_t	pervStruct;	/* the contents of PervStruct */
    ml_val_t	runTimeCompUnit; /* The run-time system compilation unit root */
    ml_val_t	mathVec;	/* The Math structure root (if defined) */
} ml_heap_hdr_t;

typedef struct {	    /* The header for a blasted object image */
    Unsigned32_t    numArenas;	/* The number of small-object arenas (one per kind) */
    Unsigned32_t    numBOKinds;	/* The number of big-object kinds */
    Unsigned32_t    numBORegions;/* The number of big-object regions in the */
				/* exporting address space. */
    bool_t	    hasCode;	/* true, if the blasted object contains code */
    ml_val_t	    rootObj;	/* The root object */
} ml_blast_hdr_t;

typedef struct {	    /* The header for the extern table */
    int		numExterns;	/* The number of external symbols */
    int		externSzB;	/* The size (in bytes) of the string table area. */
} extern_tbl_hdr_t;


typedef struct {	    /* The image of an ML virtual processor.  The live */
			    /* registers are those specified by RET_MASK, plus */
			    /* the varReg, exnCont and pc. */
    ml_val_t	sigHandler;	/* the contents of MLSignalHandler */
    ml_val_t	stdArg;
    ml_val_t	stdCont;
    ml_val_t	stdClos;
    ml_val_t	pc;
    ml_val_t	exnCont;
    ml_val_t	varReg;
    ml_val_t	calleeSave[CALLEESAVE];
} ml_vproc_image_t;


/* The heap header consists of numGens generation descriptions, each of which
 * consists of (numArenas+numBOKinds) heap_arena_hdr_t records.  After the
 * generation descriptors, there are numBORegions bo_region_info_t records,
 * which are followed by the page aligned heap image follows the heap header.
 */

typedef struct {	    /* An arena header.  This is used for both the regular */
			    /* arenas and the big-object arena of a generation. */
    int		gen;		/* the generation of this arena */
    int		objKind;	/* the kind of objects in this arena */
    Unsigned32_t offset;	/* the file position at which this arena starts. */
    union {			/* additional info */
	struct {		    /* info for regular arenas */
	    Addr_t	baseAddr;	/* the base address of this arena in the */
					/* exporting address space. */
	    Addr_t	sizeB;		/* the size of the live data in this arena */
	    Addr_t	roundedSzB;	/* the padded size of this arena in the */
					/* image file */
	}	    o;
	struct {		    /* info for the big-object arena */
	    int		numBigObjs;	/* the number of big-objects in this */
					/* generation. */
	    int		numBOPages;	/* the number of big-object pages required. */
	}	    bo;
    }		info;
} heap_arena_hdr_t;

typedef struct {	    /* a descriptor of a big-object region in the */
			    /* exporting address space */
    Addr_t	baseAddr;	/* the base address of this big-object region in */
				/* the exporting address space.  Note that this */
				/* is the address of the header, not of the */
				/* first page. */
    Addr_t	firstPage;	/* the address of the first page of the region in */
				/* the exporting address space. */
    Addr_t	sizeB;		/* the total size of this big-object region */
				/* (including the header). */
} bo_region_info_t;

typedef struct {	    /* a header for a big-object */
    int		gen;		/* the generation of this big-object */
    int		objKind;	/* the class of this big-object */
    Addr_t	baseAddr;	/* the base address of this big-object in the */
				/* exporting address space */
    Addr_t	sizeB;		/* the size of this big-object */
} bigobj_hdr_t;


/** external references **/
#define isEXTERNTAG(w)		(isDESC(w) && (GET_TAG(w) == DTAG_extern))
#define EXTERNID(w)		GET_LEN(w)

/** Pointer tagging operations **/
#define HIO_ID_BITS		8
#define HIO_ADDR_BITS		(BITS_PER_WORD-HIO_ID_BITS)
#define HIO_ADDR_MASK		((1 << HIO_ADDR_BITS) - 1)

#define HIO_TAG_PTR(id,offset)	PTR_CtoML(((id)<<HIO_ADDR_BITS)|(Addr_t)(offset))
#define HIO_GET_ID(p)		(PTR_MLtoADDR(p)>>HIO_ADDR_BITS)
#define HIO_GET_OFFSET(p)	(PTR_MLtoADDR(p) & HIO_ADDR_MASK)

#endif /* !_ML_IMAGE_ */
