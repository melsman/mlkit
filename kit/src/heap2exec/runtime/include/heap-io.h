/* heap-io.h
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * Interface to heap-io library.
 */

#ifndef _HEAP_IO_
#define _HEAP_IO_

#include <stdio.h>

extern status_t ExportHeapImage (ml_state_t *msp, FILE *file);
extern status_t ExportFnImage (ml_state_t *msp, ml_val_t funct, FILE *file);
extern ml_state_t *ImportHeapImage (const char *fname, heap_params_t *heapParams);

extern ml_val_t BlastOut (ml_state_t *msp, ml_val_t obj);
extern ml_val_t BlastIn (ml_state_t *msp, Byte_t *data, long len, bool_t *errFlg);

#endif /* _HEAP_IO_ */
