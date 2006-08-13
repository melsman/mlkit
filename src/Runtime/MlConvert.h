/***************************************************************/
/* This module implements some converter functions between the */
/* representation of values in ML and C.                       */
/* This file includes declarations and tagging also found in   */
/* Tagging.h                                                   */
/* This file is specialized to having values and integers      */
/* untagged and datatypes boxed.                               */
/***************************************************************/

#ifndef __ML_CONVERT
#define __ML_CONVERT

/*----------------------------------------------------------------*
 * Allocating into infinite regions.                              *
 *----------------------------------------------------------------*/
void resetRegion(uintptr_t rAdr);
#define is_inf_and_atbot(x) (((x) & 0x00000003)==0x00000003)
uintptr_t *alloc (uintptr_t rAdr, size_t n);

/*----------------------------------------------------------------*
 * Converting integers.                                           * 
 *----------------------------------------------------------------*/
#define convertIntToC(i)  (i)
#define convertIntToML(i) (i)

/*----------------------------------------------------------------*
 * Converting reals.                                              * 
 *----------------------------------------------------------------*/
/* extract double from storable value. 
 * get_d may be used as l-value as well. */

#define get_d(s)     (* (double *)(((size_t *)s)))
#define set_dtag(d)  /* nothing */
#define convertRealToC(mlReal)  (get_d((mlReal)))
#define convertRealToML(cReal, mlReal) {get_d((mlReal)) = (cReal);}
#define allocReal(realRho, realPtr) {realPtr = alloc(realRho,2);}
#define allocRealProf(realRho, realPtr, pPoint) {\
  realPtr = alloc(realRho, 2+sizeObjectDesc);\
  ((ObjectDesc *) realPtr)->atId = pPoint; \
  ((ObjectDesc *) realPtr)->size = 2; /* Size is two words. */ \
  realPtr = (uintptr_t *)(((ObjectDesc *)realPtr)+1); \
}

/*----------------------------------------------------------------*
 * Converting booleans.                                           * 
 *----------------------------------------------------------------*/
#define mlTRUE 3
#define mlFALSE 1
#define cTRUE 1
#define cFALSE 0
#define convertBoolToC(i)  ((i) >> 1)
#define convertBoolToML(i) (((i) << 1) + 1)

/*----------------------------------------------------------------*
 * Converting units.                                              *
 *----------------------------------------------------------------*/
#define mlUNIT 1
#define convertUnitToC(u) ()
#define convertUnitToML(u) (mlUNIT)

/*----------------------------------------------------------------*
 * Converting records.                                            * 
 *   mlTagRecordML: Store tag in record at address recAddr.       *
 *   elemRecordML: mem[recAddr+offset]=mlVal, where mlVal         *
 *                 has to be the value in ML representation.      *
 *                 First elem in record has offset 0.             *
 *----------------------------------------------------------------*/
#define elemRecordML(recAddr, offset) (*((size_t *)(recAddr)+(offset)))
#define first(x)   (*(size_t *)(x))
#define second(x)  (*((size_t *)(x)+1))

#define allocRecordML(rhoRec, size, recAddr) {recAddr= (size_t *) alloc(rhoRec, size);}
#define allocRecordMLProf(rhoRec, ssize, recAddr, pPoint) { \
   recAddr = (size_t *) alloc(rhoRec, ssize+sizeObjectDesc); \
   ((ObjectDesc *) recAddr)->atId = pPoint; \
   ((ObjectDesc *) recAddr)->size = ssize; \
   recAddr = (size_t *)(((ObjectDesc *)recAddr)+1); \
							 }

/*----------------------------------------------------------------*
 * Converting strings.                                            *
 *   String converting is implemented in file                     *
 *     /src/Runtime/Version#/String.c                             *
 *----------------------------------------------------------------*/
#define valueTagString  1
typedef size_t StringDesc;
void printString(StringDesc *str);
void convertStringToC(StringDesc *mlStr, char *cStr, size_t cStrLen, uintptr_t exn);
StringDesc *convertStringToML(Region rhoString, char *cStr);
StringDesc *convertStringToMLProfiling(Region rhoString, char *cStr, size_t pPoint);
size_t sizeStringML(StringDesc *str);

/*-------------------------------------------------------------------*
 * Construction of (converting) lists.                               *
 *   The lists has to be constructed by hand. An algorithm for       *
 *   constructing the list backwards are as follows:                 *
 *                 makeNIL(consRho,resList);                         *
 *                 while (more elements) {                           *
 *                 ml_elem = ...;                                    *
 *                 allocRecordML(pairRho, 2, pair);                  *
 *                 first(pair) = (int) ml_elem;                      *
 *                 second(pair) = (int) resList;                     *
 *                 makeCONS(consRho, pair, resList);                 *
 *                 }                                                 *
 *                 return (int) resList;                             *
 *                                                                   *
 *                                                                   *
 *  A list can also (less elegant) be constructed forwards:          *
 *    if list.size = 0                                               *
 *      resList = mkNil(consRho)                                     *
 *    else                                                           *
 *      ml_elem = ...                                                *
 *      allocRecordML(pairRho, 2, pair);                             *
 *      first(pair) = (int) ml_elem;                                 *
 *      makeCONS(consRho, pair, cons);                               *
 *      res = (int) cons;                                            *
 *                                                                   *
 *      while (more elements) {                                      *
 *      ml_elem = ...                                                *
 *      allocRecordML(pairRho, 2, temp_pair);                        *
 *      first(temp_pair) = (int) ml_elem;                            *
 *      makeCONS(consRho, temp_pair, cons);                          *
 *      second(pair) = (int) cons;                                   *
 *      pair = temp_pair;                                            *
 *      }                                                            *
 *      makeNIL(consRho, cons);                                      *
 *      second(pair) = (int)cons;                                    *
 *      return res;                                                  *
 * The algorithm is explained in the book-let:                       *
 *     Programming With Regions in the ML Kit                        *
 * found in the doc directory.                                       *
 *-------------------------------------------------------------------*/
#define valueTagCon0    2
#define valueTagCon1    3

#define contag(x)  (*(uintptr_t *)x)            /* Constructor tag of a value.   */
#define conarg(x)  (*((uintptr_t *)(x)+1))      /* Constructor arg of a value.   */
#define NIL        (8*0+valueTagCon0)     /* Tag for a NIL constructor.    */
#define CONS       (8*0+valueTagCon1)     /* Tag for a CONS constructor.   */
#define isNIL(x)   (contag(x) == NIL)     /* Is the con. tag NIL.          */
#define isCONS(x)  (contag(x) == CONS)    /* Is the con. tag CONS.         */

#define makeNIL(rAddr, ptr) {ptr = alloc(rAddr, 1);\
                             contag(ptr) = NIL;}

#define makeCONS(rAddr, pair, ptr) {ptr = alloc(rAddr, 2);\
				    contag(ptr) = CONS;\
				    conarg(ptr) = (uintptr_t) pair;}

typedef struct objectDesc {
  size_t atId;               /* Allocation point identifier. */
  size_t size;               /* Size of object in bytes. */
} ObjectDesc;
#define sizeObjectDesc (sizeof(ObjectDesc)/(sizeof(void *)))

#define makeNILProf(rAddr, ptr, pPoint) {\
  ptr = alloc(rAddr, 1+sizeObjectDesc);\
  ((ObjectDesc *) ptr)->atId = pPoint; \
  ((ObjectDesc *) ptr)->size = 1; /* Size is one word. */ \
  ptr = (uintptr_t *)(((ObjectDesc *)ptr)+1); \
  contag(ptr) = NIL;\
}

#define makeCONSProf(rAddr, pair, ptr, pPoint) {\
  ptr = alloc(rAddr, 2+sizeObjectDesc);\
  ((ObjectDesc *) ptr)->atId = pPoint; \
  ((ObjectDesc *) ptr)->size = 2; /* Size is two words. */ \
  ptr = (uintptr_t *)(((ObjectDesc *)ptr)+1); \
  contag(ptr) = CONS; \
  conarg(ptr) = (uintptr_t) pair; \
}

#define hd(x)      (first(conarg(x)))     /* Head of a list.               */
#define tl(x)      (second(conarg(x)))    /* Tail of a list.               */

/*----------------------------------------------------------------*
 * Exceptions.                                                    * 
 *----------------------------------------------------------------*/
void raise_exn(uintptr_t exn);

#endif /*__ML_CONVERT*/
