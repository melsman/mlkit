/*----------------------------------------------------------------*
 *                    Tagging operations                          *
 *----------------------------------------------------------------*/
#ifndef TAGGING
#define TAGGING

#include "Flags.h"
#include "Region.h"


#define valTag(x)         (*(int *)x)
#define valTagKind(x)     ((*(int *)x) & 7)
#define valueTagString  1
#define valueTagCon0    2
#define valueTagCon1    3

#if TAG_VALUES
#define valueTagReal    0
#define valueTagRecord  4
#define valueTagRef     5
#define tag_record(x)     (*(int *)x)
#define size_record(x)    (tag_record(x) >> 3)
#endif


#define mlTRUE 3
#define mlFALSE 1
#define mlUNIT 1

#define TRUE 1
#define FALSE 0

/*----------------------------------------------------------------*
 * Converting integers.                                           * 
 *----------------------------------------------------------------*/

#if TAG_INTEGERS
#define convertIntToC(i)  ((i) >> 1)
#define convertIntToML(i) (((i) << 1) + 1)
#else
#define convertIntToC(i)  (i)
#define convertIntToML(i) (i)
#endif

/*----------------------------------------------------------------*
 * Converting reals.                                              * 
 *----------------------------------------------------------------*/

/* extract double from storable value. 
 * get_d may be used as l-value as well. */

#if TAG_VALUES
#define get_d(s)     (* (double *)(((int *)s)+2))
#define set_dtag(d)  (* (int *)(d) = valueTagReal)
#define convertRealToC(mlReal)  (get_d((mlReal)))
#define convertRealToML(cReal, mlReal) {get_d((mlReal)) = (cReal);\
					set_dtag((mlReal));}
#else
#define get_d(s)     (* (double *)(((int *)s)))
#define set_dtag(d)  /* nothing */
#define convertRealToC(mlReal)  (get_d((mlReal)))
#define convertRealToML(cReal, mlReal) {get_d((mlReal)) = (cReal);}
#endif

/*----------------------------------------------------------------*
 * Converting booleans.                                           * 
 *----------------------------------------------------------------*/
#define convertBoolToC(i)  ((i) >> 1)
#define convertBoolToML(i) (((i) << 1) + 1)

/*----------------------------------------------------------------*
 * Converting units.                                              *
 *----------------------------------------------------------------*/
#define convertUnitToC(u) ()
#define convertUnitToML(u) (mlUNIT)

/*----------------------------------------------------------------*
 * Converting records.                                            * 
 *   mlTagRecordML: Store tag in record at address recAddr.       *
 *   storeElemRecordML: mem[recAddr+offset]=mlVal, where mlVal    *
 *                      has to be the value in ML representation. *
 *                      First elem in record has offset 0.        *
 *----------------------------------------------------------------*/

#if TAG_VALUES
#define mkTagRecordML(recAddr, size)  (*((int *)recAddr) = (size)*8+valueTagRecord)
#define storeElemRecordML(recAddr, offset, mlVal) (*(((int *)recAddr)+(offset+1))=(mlVal))
#define allocRecordML(rAddr, size, recAddr) {recAddr=alloc(rAddr, size+1);\
        				     mkTagRecordML(recAddr, size);}
#define first(x)   (*((int *)(x)+1))
#define second(x)  (*((int *)(x)+2))

#ifdef PROFILING
#define allocRecordMLProf(rAddr, ssize, recAddr, pPoint) { \
   recAddr = alloc(rAddr, ssize+1+sizeObjectDesc); \
   ((ObjectDesc *) recAddr)->atId = pPoint; \
   ((ObjectDesc *) recAddr)->size = ssize+1; \
   recAddr = (int *)(((ObjectDesc *)recAddr)+1); \
   mkTagRecordML(recAddr, ssize); \
}
#endif /*PROFILING*/

#else /* don't tag values */
#define mkTagRecordML(recAddr, size)
#define storeElemRecordML(recAddr, offset, mlVal) (*(((int *)recAddr)+(offset))=(mlVal))
#define allocRecordML(rAddr, size, recAddr) {recAddr=alloc(rAddr, size);}
#define first(x)   (*(int *)(x))
#define second(x)  (*((int *)(x)+1))

#ifdef PROFILING
#define allocRecordMLProf(rAddr, ssize, recAddr, pPoint) { \
   recAddr = alloc(rAddr, ssize+sizeObjectDesc); \
   ((ObjectDesc *) recAddr)->atId = pPoint; \
   ((ObjectDesc *) recAddr)->size = ssize; \
   recAddr = (int *)(((ObjectDesc *)recAddr)+1); \
}
#endif /*PROFILING*/

#endif /*NO TAG_VALUES*/

/*----------------------------------------------------------------*
 * Operations on Lists                                            *
 *----------------------------------------------------------------*/

#define contag(x)  (*(int *)x)            /* Constructor tag of a value.   */
#define conarg(x)  (*((int *)(x)+1))      /* Constructor arg of a value.   */
#define NIL        (8*0+valueTagCon0)     /* Tag for a NIL constructor.    */
#define CONS       (8*0+valueTagCon1)     /* Tag for a CONS constructor.   */
#define isNIL(x)   (contag(x) == NIL)     /* Is the con. tag NIL.          */
#define isCONS(x)  (contag(x) == CONS)    /* Is the con. tag CONS.         */

#define makeNIL(rAddr, ptr) {ptr = alloc(rAddr, 1);\
                             contag(ptr) = NIL;}

#define makeCONS(rAddr, pair, ptr) {ptr = alloc(rAddr, 2);\
				    contag(ptr) = CONS;\
				    conarg(ptr) = (int) pair;}

#ifdef PROFILING

#define sizeObjectDesc (sizeof(ObjectDesc)/4)

#define makeNILProf(rAddr, ptr, pPoint) {\
  ptr = alloc(rAddr, 1+sizeObjectDesc);\
  ((ObjectDesc *) ptr)->atId = pPoint; \
  ((ObjectDesc *) ptr)->size = 1; /* Size is one word. */ \
  ptr = (int *)(((ObjectDesc *)ptr)+1); \
  contag(ptr) = NIL;\
}

#define makeCONSProf(rAddr, pair, ptr, pPoint) {\
  ptr = alloc(rAddr, 2+sizeObjectDesc);\
  ((ObjectDesc *) ptr)->atId = pPoint; \
  ((ObjectDesc *) ptr)->size = 2; /* Size is two words. */ \
  ptr = (int *)(((ObjectDesc *)ptr)+1); \
  contag(ptr) = CONS; \
  conarg(ptr) = (int) pair; \
}

#endif /*PROFILING*/

#define hd(x)      (first(conarg(x)))     /* Head of a list.               */
#define tl(x)      (second(conarg(x)))    /* Tail of a list.               */


#endif /*TAGGING*/
