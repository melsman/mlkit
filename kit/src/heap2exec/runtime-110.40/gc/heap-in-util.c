/* heap-in-util.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * Utility routines to import an ML heap image.
 */

#include "ml-base.h"
#include "heap.h"
#include "ml-values.h"
#include "ml-heap-image.h"
#include "c-globals-tbl.h"
#include "heap-input.h"

/* local routines */
PVT status_t ReadBlock (FILE *file, void *blk, long len);


/* HeapIO_ReadExterns:
 */
ml_val_t *HeapIO_ReadExterns (inbuf_t *bp)
{
    extern_tbl_hdr_t	hdr;
    ml_val_t		*externs;
    Byte_t		*buf, *cp;
    int			i;

  /* Read the header */
    HeapIO_ReadBlock(bp, &(hdr), sizeof(hdr));

    externs = NEW_VEC(ml_val_t, hdr.numExterns);

  /* Read in the names of the exported symbols */
    buf = NEW_VEC(Byte_t, hdr.externSzB);
    HeapIO_ReadBlock (bp, buf, hdr.externSzB);

  /* map the names of the external symbols to addresses in the run-time system */
    for (cp = buf, i = 0;  i < hdr.numExterns;  i++) {
	if ((externs[i] = ImportCSymbol ((char *)cp)) == ML_unit)
	    Die ("Run-time system does not provide \"%s\"", cp);
	cp += (strlen(cp) + 1);
    }
    FREE (buf);

    return externs;

} /* end of HeapIO_ReadExterns */


/* HeapIO_Seek:
 *
 * Adjust the next character position to the given position in the
 * input stream.
 */
status_t HeapIO_Seek (inbuf_t *bp, long offset)
{
    if (bp->file == NULL) {
      /* the stream is in-memory */
	Byte_t	*newPos = bp->base + offset;
	if (bp->buf + bp->nbytes <= newPos)
	    return FAILURE;
	else {
	    bp->nbytes -= (newPos - bp->buf);
	    bp->buf = newPos;
	    return SUCCESS;
	}
    }
    else {
	Die ("HeapIO_Seek");
    }

} /* end of HeapIO_Seek */


/* HeapIO_ReadBlock:
 */
status_t HeapIO_ReadBlock (inbuf_t *bp, void *blk, long len)
{
    status_t	sts = SUCCESS;

    if (bp->nbytes == 0) {
	if (bp->file != NULL)
	    sts = ReadBlock (bp->file, blk, len);
	else {
	    Error ("missing data in memory blast object");
	    return FAILURE;
	}
    }
    else if (bp->nbytes >= len) {
	memcpy (blk, bp->buf, len);
	bp->nbytes -= len;
	bp->buf += len;
    }
    else {
	memcpy (blk, bp->buf, bp->nbytes);
	sts = ReadBlock (bp->file, ((Byte_t *)blk) + bp->nbytes, len - bp->nbytes);
	bp->nbytes = 0;
    }

    if (bp->needsSwap) {
	Die ("byte-swapping not implemented yet");
    }

    return sts;

} /* end of HeapIO_ReadBlock */

/* ReadBlock:
 */
PVT status_t ReadBlock (FILE *file, void *blk, long len)
{
    int		sts;
    Byte_t	*bp = (Byte_t *)blk;

    while (len > 0) {
	sts = fread (bp, 1, len, file);
	len -= sts;
	bp += sts;
	if ((sts < len) && (ferror(file) || feof(file))) {
	    Error ("unable to read %d bytes from image\n", len);
	    return FAILURE;
	}
    }

    return SUCCESS;

} /* end of ReadBlock. */
