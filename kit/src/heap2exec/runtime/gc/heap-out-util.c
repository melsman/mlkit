/* heap-out-util.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * Utility routines to export (or blast) an ML heap image.
 */

#include "ml-base.h"
#include "heap.h"
#include "ml-values.h"
#include "ml-heap-image.h"
#include "c-globals-tbl.h"
#include "heap-output.h"


/* HeapIO_WriteImageHeader:
 *
 *  Blast out the ml_image_hdr_t.
 */
status_t HeapIO_WriteImageHeader (writer_t *wr, int kind)
{
    ml_image_hdr_t	hdr;

    hdr.byteOrder = ORDER;
    hdr.magic	  = ((kind == EXPORT_HEAP_IMAGE) || (kind == EXPORT_FN_IMAGE))
			? IMAGE_MAGIC : BLAST_MAGIC;
    hdr.kind	  = kind;
    /* hdr.arch[] */
    /* hdr.opsys[] */

    WR_Write(wr, &hdr, sizeof(hdr));
    if (WR_Error(wr))
	return FAILURE;
    else
	return SUCCESS;

} /* end of HeapIO_WriteImageHeader */


/* HeapIO_WriteExterns:
 *
 * Write out the external symbol table, returning the number of bytes
 * written (-1 on error).
 */
Addr_t HeapIO_WriteExterns (writer_t *wr, export_table_t *tbl)
{
    int			i, numExterns;
    export_item_t	*externs;
    extern_tbl_hdr_t	hdr;
    Addr_t		strSize, nbytes = sizeof(extern_tbl_hdr_t), padSzB;

    ExportedSymbols (tbl, &numExterns, &externs);

    for (strSize = 0, i = 0;  i < numExterns;  i++)
	strSize += (strlen(externs[i]) + 1);
  /* include padding to WORD_SZB bytes */
    padSzB = ROUNDUP(strSize, WORD_SZB) - strSize;
    strSize += padSzB;
    nbytes += strSize;

  /* write out the header */
    hdr.numExterns = numExterns;
    hdr.externSzB = strSize;
    WR_Write(wr, &hdr, sizeof(hdr));

  /* write out the external symbols */
    for (i = 0;  i < numExterns;  i++) {
	WR_Write (wr, externs[i], strlen(externs[i])+1);
    }

  /* write the padding */
    {
	char	pad[8] = {0, 0, 0, 0, 0, 0, 0, 0};
	if (padSzB != 0) {
	    WR_Write (wr, pad, padSzB);
	}
    }

  done:;
    FREE (externs);

    if (WR_Error(wr))
	return -1;
    else
	return nbytes;

} /* end of HeapIO_WriteExterns */

