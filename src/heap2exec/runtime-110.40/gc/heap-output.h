/* heap-output.h
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 */

#ifndef _HEAP_OUTPUT_
#define _HEAP_OUTPUT_

#ifndef _WRITER_
#include "writer.h"
#endif

extern status_t HeapIO_WriteImageHeader (writer_t *wr, int kind);
extern Addr_t HeapIO_WriteExterns (writer_t *wr, export_table_t *tbl);

#endif /* !_HEAP_OUTPUT_ */
