/* NOT UPDATED 10/01/1999, Niels */

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
void resetRegion(int rAdr);
#define is_inf_and_atbot(x) (((x) & 0x00000003)==0x00000003)
int *alloc (int rAdr, int n);

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
#define get_d(s)     (* (double *)(((int *)s)))
#define set_dtag(d)  /* nothing */
#define convertRealToC(mlReal)  (get_d((mlReal)))
#define convertRealToML(cReal, mlReal) {get_d((mlReal)) = (cReal);}
#define allocReal(realRho, realPtr) {realPtr = alloc(realRho,2);}
#define allocRealProf(realRho, realPtr, pPoint) {\
  realPtr = alloc(realRho, 2+sizeObjectDesc);\
  ((ObjectDesc *) realPtr)->atId = pPoint; \
  ((ObjectDesc *) realPtr)->size = 2; /* Size is two words. */ \
  realPtr = (int *)(((ObjectDesc *)realPtr)+1); \
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
#define elemRecordML(recAddr, offset) (*((int *)(recAddr)+(offset)))
#define first(x)   (*(int *)(x))
#define second(x)  (*((int *)(x)+1))

#define allocRecordML(rhoRec, size, recAddr) {recAddr=alloc(rhoRec, size);}
#define allocRecordMLProf(rhoRec, ssize, recAddr, pPoint) { \
   recAddr = alloc(rhoRec, ssize+sizeObjectDesc); \
   ((ObjectDesc *) recAddr)->atId = pPoint; \
   ((ObjectDesc *) recAddr)->size = ssize; \
   recAddr = (int *)(((ObjectDesc *)recAddr)+1); \
							 }

/*----------------------------------------------------------------*
 * Converting strings.                                            *
 *   String converting is implemented in file                     *
 *     /src/Runtime/Version#/String.c                             *
 *----------------------------------------------------------------*/
#define valueTagString  1
typedef int StringDesc;
void printString(StringDesc *str);
void convertStringToC(StringDesc *mlStr, char *cStr, int cStrLen, int exn);
StringDesc *convertStringToML(int rhoString, char *cStr);
StringDesc *convertStringToMLProfiling(int rhoString, char *cStr, int pPoint);
int sizeStringML(StringDesc *str);

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

typedef struct objectDesc {
  int atId;               /* Allocation point identifier. */
  int size;               /* Size of object in bytes. */
} ObjectDesc;
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

#define hd(x)      (first(conarg(x)))     /* Head of a list.               */
#define tl(x)      (second(conarg(x)))    /* Tail of a list.               */

/*----------------------------------------------------------------*
 * Exceptions.                                                    * 
 *----------------------------------------------------------------*/
void raise_exn(int exn);

#endif /*__ML_CONVERT*/
