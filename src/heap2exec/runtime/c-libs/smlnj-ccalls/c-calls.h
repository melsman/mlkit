/* c-calls.h 
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 *
 */

#ifndef _C_CALLS_
#define _C_CALLS_

#define N_ARGS 15 /* max number of args a ML callable C function may have */

#ifndef _ASM_

#include "ml-sizes.h"

/* malloc's should return sufficiently aligned blocks */
#define HAS_ALIGNED_MALLOC
#if defined(HAS_ALIGNED_MALLOC)
#include <stdlib.h>
#include <malloc.h>

#define memalign(align,sz) malloc(sz)
#endif

#include <string.h>

extern Word_t *checked_memalign(int n,int align);
#define checked_alloc(n) checked_memalign((n),(1))

extern Word_t mk_C_function(ml_state_t *msp,
			    ml_val_t f,
			    int nargs,char *argtypes[],char *rettype);

extern ml_val_t datumCtoML(ml_state_t *msp,char *type,Word_t p,ml_val_t *root);
extern int datumMLtoC(ml_state_t *msp,char **t,Word_t **p,ml_val_t ret);
extern ml_val_t revMLList(ml_val_t l,ml_val_t acc);

extern ml_state_t *visible_msp;
#endif

#endif /* !_C_CALLS_ */

