/*----------------------------------------------------------------*
 *                         Lists                                  *
 *----------------------------------------------------------------*/

#ifndef LIST
#define LIST

/*----------------------------------------------------------------*
 * Include files                                                  *
 * Compiling: cc -Aa -c List.c                                    *
 *----------------------------------------------------------------*/
#include "Flags.h"
#include "Tagging.h"
#include "Region.h"

/*------------------------------------------------------------------------*
 * We have to know how lists are represented in the backend.              *
 *   A  NIL vector consists of 1 word: NILtag                             *
 *   A CONS vector consists of 2 words: CONStag | arg                     *
 * arg is a represented as a tuple. Use only tuple operations (first and  *
 * second) for accessing arguments to CONS.                               *
 *------------------------------------------------------------------------*/

#if UNBOX_LISTS
#define NIL 3
#define CONS 0
#define isNIL(x)        ((x) == NIL)
#define isCONS(x)       (((x) & 3) == CONS)
#define conarg(x)       (x)                   /* the cons tag is zero! */
#define makeNil(ptr)    {ptr = (int *)NIL;}
#define makeCONS(pair,ptr)  {ptr = pair;}

#else /* don't unbox lists */

#define contag(x)  (*(int *)x)            /* Constructor tag of a value.   */
#define conarg(x)  (*((int *)(x)+1))      /* Constructor arg of a value.   */

#define NIL        (8*0+valueTagCon0)     /* Tag for a NIL constructor.    */
#define CONS       (8*0+valueTagCon1)     /* Tag for a CONS constructor.   */
#define isNIL(x)   (contag(x) == NIL)     /* Is the con. tag NIL.          */
#define isCONS(x)  (contag(x) == CONS)    /* Is the con. tag CONS.         */

/* Operations for constructing lists. */

#define makeNil(rAddr, ptr) {ptr = alloc(rAddr, 1);\
                             contag(ptr) = NIL;}

#define makeCONS(rAddr, pair, ptr) {ptr = alloc(rAddr, 2);\
				    contag(ptr) = CONS;\
				    conarg(ptr) = (int) pair;}


/***************************************************************************
 *     Changed runtime operations for making profiling possible.           *
 *                                                                         *
 * Only defined when not unboxing lists...                                 *
 *                                                                         *
 * makeNilProfiling(rAddr, ptr, pPoint)                                    *
 * makeCONSProfiling(rAddr, pair, ptr, pPoint)                             *
 ***************************************************************************/
#ifdef PROFILING

#define makeNilProfiling(rAddr, ptr, pPoint) {\
  ptr = alloc(rAddr, 1+sizeObjectDesc);\
  ((ObjectDesc *) ptr)->atId = pPoint; \
  ((ObjectDesc *) ptr)->size = 1; /* Size is one word. */ \
  ptr = (int *)(((ObjectDesc *)ptr)+1); \
  contag(ptr) = NIL;\
}

#define makeCONSProfiling(rAddr, pair, ptr, pPoint) {\
  ptr = alloc(rAddr, 2+sizeObjectDesc);\
  ((ObjectDesc *) ptr)->atId = pPoint; \
  ((ObjectDesc *) ptr)->size = 2; /* Size is two words. */ \
  ptr = (int *)(((ObjectDesc *)ptr)+1); \
  contag(ptr) = CONS; \
  conarg(ptr) = (int) pair; \
}
#endif /*UNBOX_LISTS*/

#endif /*PROFILING*/

#define hd(x)      (first(conarg(x)))     /* Head of a list.               */
#define tl(x)      (second(conarg(x)))    /* Tail of a list.               */

#endif /*LIST*/
