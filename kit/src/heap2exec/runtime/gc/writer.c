/* writer.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * An implementation of the abstract writers on top of ANSI C streams.
 */

#include <stdio.h>
#include "ml-base.h"
#include "writer.h"

#ifndef SEEK_SET
#  define SEEK_SET	0
#endif

PVT void Put (writer_t *wr, Word_t w);
PVT void Write (writer_t *wr, const void *data, Addr_t nbytes);
PVT void Flush (writer_t *wr);
PVT long Tell (writer_t *wr);
PVT void Seek (writer_t *wr, long offset);
PVT void Free (writer_t *wr);

#define FileOf(wr)	((FILE *)((wr)->data))

/* WR_OpenFile:
 *
 * Open a file for writing, and make a writer for it.
 */
writer_t *WR_OpenFile (FILE *f)
{
    writer_t	*wr;

    if (f == NULL)
	return NIL(writer_t *);

    wr = NEW_OBJ(writer_t);
    wr->errFlg	= FALSE;
    wr->data	= (void *)f;
    wr->putWord	= Put;
    wr->write	= Write;
    wr->flush	= Flush;
    wr->tell	= Tell;
    wr->seek	= Seek;
    wr->free	= Free;

    return wr;

} /* end of WR_OpenFile */

/* Put:
 */
PVT void Put (writer_t *wr, Word_t w)
{
    FILE	*f = FileOf(wr);

    if (fwrite((void *)&w, WORD_SZB, 1, f) != 1) {
	wr->errFlg = TRUE;
    }

} /* end of Put */

/* Write:
 */
PVT void Write (writer_t *wr, const void *data, Addr_t nbytes)
{
    FILE	*f = FileOf(wr);

    if (fwrite(data, 1, nbytes, f) != nbytes) {
	wr->errFlg = TRUE;
    }

} /* end of Write */

/* Flush:
 */
PVT void Flush (writer_t *wr)
{
    fflush (FileOf(wr));

} /* end of Flush */

/* Tell:
 */
PVT long Tell (writer_t *wr)
{
    return ftell(FileOf(wr));

} /* end of Tell */

/* Seek:
 */
PVT void Seek (writer_t *wr, long offset)
{
    if (fseek(FileOf(wr), offset, SEEK_SET) != 0)
	wr->errFlg = TRUE;

} /* end of Seek */

/* Free:
 */
PVT void Free (writer_t *wr)
{
    fflush (FileOf(wr));
    FREE(wr);

} /* end of Free */
