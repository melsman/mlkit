/* heap-input.h
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 */

#ifndef _HEAP_INPUT_
#define _HEAP_INPUT_

#include <stdio.h>

#ifndef _ADDR_HASH_
#include "addr-hash.h"
#endif

typedef struct {	    /* An input source for reading heap data.  We need */
			    /* this because the blaster may need to read from a */
			    /* stream that has already had characters read from it. */
    bool_t	needsSwap;	/* true, if the input bytes need to be swapped */
    FILE	*file;		/* the file descriptor to read from, once the */
				/* buffered characters are exhausted */
    Byte_t	*base;		/* the start of the bufferec characters */
    Byte_t	*buf;		/* the current position in the buffer */
    long	nbytes;
} inbuf_t;


/** Big-object relocation info **/

typedef struct {	/* big-object relocation info */
    Addr_t	    oldAddr;
    bigobj_desc_t   *newObj;
} bo_reloc_t;

typedef struct {	/* big-object region relocation info */
    Addr_t	    firstPage;	/* the address of the first page of the region */
    int		    nPages;	/* the number of pages in the region */
    bo_reloc_t	    **objMap;   /* the map from pages to big-object relocation */
				/* info. */
} bo_region_reloc_t;

/* Big-object region hash table interface */
#define LookupBORegion(tbl, bibopIndex)	\
	((bo_region_reloc_t *)AddrTblLookup(tbl, BIBOP_INDEX_TO_ADDR(bibopIndex)))

/* Utility routines */
extern ml_val_t *HeapIO_ReadExterns (inbuf_t *bp);
extern status_t HeapIO_Seek (inbuf_t *bp, long offset);
extern status_t HeapIO_ReadBlock (inbuf_t *bp, void *blk, long len);

#endif /* !_HEAP_INPUT_ */
