/* writer.h
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This is an abstraction of a buffered output device for writing
 * heap data.
 */

#ifndef _WRITER_
#define _WRITER_

#include <stdio.h>  /* for FILE */

typedef struct writer {
    bool_t	errFlg;
    void	*data;
    void	(*putWord)(struct writer *, Word_t);
    void	(*write)(struct writer *, const void *, Addr_t);
    void	(*flush)(struct writer *);
    long	(*tell)(struct writer *);
    void	(*seek)(struct writer *, long offset);
    void	(*free)(struct writer *);
} writer_t;

/* open a file for writing, and make a file for it */
extern writer_t *WR_OpenFile (FILE *file);
/* make a writer from a region of memory */
extern writer_t *WR_OpenMem (Byte_t *data, Addr_t len);

#define WR_Error(wr)			((wr)->errFlg)
#define WR_Put(wr, w)			((wr)->putWord((wr), (w)))
#define WR_Write(wr, data, nbytes)	((wr)->write((wr), (data), (nbytes)))
#define WR_Flush(wr)			((wr)->flush(wr))
#define WR_Tell(wr)			((wr)->tell(wr))
#define WR_Seek(wr, offset)		((wr)->seek((wr), (offset)))
#define WR_Free(wr)			((wr)->free(wr))

#endif /* !_WRITER_ */
