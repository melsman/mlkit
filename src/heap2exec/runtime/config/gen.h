/* gen.h
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 */

#ifndef _GEN_COMMON_
#define _GEN_COMMON_

#include <stdio.h>

extern FILE *OpenFile (char *fname, char *flag);
extern void CloseFile (FILE *f, char *flag);

#ifndef _ML_BASE_
/* nil pointers */
#define NIL(ty)		((ty)0)

/* aliases for malloc/free, so that we can easily replace them */
#define MALLOC(sz)	malloc(sz)
#define FREE(p)		free(p)

/* Allocate a new C object of type t. */
#define NEW_OBJ(t)	((t *)MALLOC(sizeof(t)))
/* Allocate a new C array of type t objects. */
#define NEW_VEC(t,n)	((t *)MALLOC((n)*sizeof(t)))
#endif /* !_ML_BASE_ */

#endif /* !_GEN_COMMON_ */

