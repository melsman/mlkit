/*----------------------------------------------------------------*
 *                         Lists                                  *
 *----------------------------------------------------------------*/

#ifndef __LIST_H
#define __LIST_H

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

#define NIL 3
#define CONS 0
#define isNIL(x)        ((x) == NIL)
#define isCONS(x)       (((x) & 3) == CONS)
#define conarg(x)       (x)                   /* the cons tag is zero! */
#define makeNIL(ptr)    {ptr = (uintptr_t *)NIL;}
#define makeCONS(pair,ptr)  {ptr = pair;}

#define hd(x)      (first(conarg(x)))     /* Head of a list.               */
#define tl(x)      (second(conarg(x)))    /* Tail of a list.               */

#endif /* __LIST_H */
