/*----------------------------------------------------------------*
 *                    Tagging operations                          *
 *----------------------------------------------------------------*/
#ifndef __TAGGING_H
#define __TAGGING_H

#include "Flags.h"
#include "Region.h"

#define tag_kind(x)                 ((x) & 0x1F)         /* Least 5 significant bits    */
#define val_tag(x)                  (*(uintptr_t *)x)
#define val_tag_kind(x)             ((*(uintptr_t *)x) & 0x1F) /* Least 5 significant bits    */
#define is_const(x)                 (((unsigned long)x) & 0x20)  /* Bit 6 is the constant bit   */
#define set_tag_const(x)            ((x) | 0x20)         /* Set bit 6, the constant bit */
#define clear_tag_const(x)          ((x) & (UINTPTR_MAX ^ 0x20))   /* Clear bit 6                 */
// #define clear_tag_const(x)          ((x) & 0xFFFFFFDF)   /* Clear bit 6                 */
#define val_tag_kind_const(x)       ((*(uintptr_t *)x) & 0x3F) /* Least 6 significant bits    */

#define gen_record_tag(s,o,i,t) (((((s)<<19) | ((o)<<6)) | ((i)<<5)) | (t))
#define gen_string_tag(s,i,t)   ((((s)<<6) | ((i)<<5)) | (t))

#define TAG_RECORD       0x06
#define TAG_RECORD_CONST 0x26
#define TAG_STRING       0x01
#define TAG_STRING_CONST 0x21
#define TAG_CON0         0x02
#define TAG_CON0_CONST   0x22
#define TAG_CON1         0x03
#define TAG_CON1_CONST   0x23
#define TAG_REF          0x05
#define TAG_REF_CONST    0x25
#define TAG_TABLE        0x07
#define TAG_TABLE_CONST  0x27

// size field in table is the size in words (excluding tag), size
// field in string is the size in bytes (excluding tag)

#define is_string(x)        (tag_kind(x) == TAG_STRING)
#define is_table(x)         (tag_kind(x) == TAG_TABLE)

#define val_tag_string(s)   (gen_string_tag((s),0,1))
#define get_string_size(s)  ((s) >> 6)
#define val_tag_con0(c_tag) (gen_string_tag((c_tag),0,2))
#define val_tag_con1(c_tag) (gen_string_tag((c_tag),0,3))
#define get_record_size(s)  ((size_t)(((size_t)(s)) >> 19))
#define get_record_skip(s)  (((size_t)(((size_t)(s)) >> 6)) & (0x1FFF))
#define val_tag_table(s)    (gen_string_tag((s),0,7))
#define get_table_size(s)   ((s) >> 6)

#ifdef TAG_VALUES
#define val_tag_real              (gen_record_tag(2,2,0,6))
#define val_tag_record(s)         (gen_record_tag((s),0,0,6))
#define val_tag_scalar_record(s)  (gen_record_tag((s),(s),0,6))
#define val_tag_ref               (gen_string_tag(0,0,5))
#define val_tag_clos(s,o)         (gen_record_tag((s),(o),0,6))
#define val_tag_sclos(s,o)        (gen_record_tag((s),(o),0,6))
#define val_tag_regvec(s)         (gen_record_tag((s),(s),0,6))
#define val_tag_exname            (gen_record_tag(2,2,0,6))
#define val_tag_excon0            (gen_record_tag(1,0,0,6))
#define val_tag_excon1            (gen_record_tag(2,0,0,6))
#define val_tag_i32b              (gen_record_tag(1,1,0,6))
#define val_tag_i64b              (gen_record_tag(1,1,0,6))
#endif

#define mlTRUE 3
#define mlFALSE 1
#define mlUNIT 1

#undef TRUE
#define TRUE 1
#undef FALSE
#define FALSE 0

/*----------------------------------------------------------------*
 * Converting integers.                                           *
 *----------------------------------------------------------------*/

#define i32ub_to_i31(i)   (((i) << 1) + 1)
#define i31_to_i32ub(i)   ((i) >> 1)
#define i64ub_to_i63(i)   (((i) << 1) + 1)
#define i63_to_i64ub(i)   ((i) >> 1)

#ifdef TAG_VALUES
#define convertIntToC(i)  ((i) >> 1)
#define convertIntToML(i) (signed long int)(((unsigned long int)(i) << 1) + 1)
#define get_i32b(b)       (* (((size_t *)b)+1))
#define set_i32b_tag(b)   (* (size_t *)(b) = val_tag_i32b)
#define get_i64b(b)       (* (((size_t *)b)+1))
#define set_i64b_tag(b)   (* (size_t *)(b) = val_tag_i64b)
#else
#define convertIntToC(i)  (i)
#define convertIntToML(i) (i)
#endif

/*----------------------------------------------------------------*
 * Tagging Scalars (used for non ML values)                       *
 *----------------------------------------------------------------*/
#ifdef TAG_VALUES
#define tag_scalar(s)      (((size_t)(s)) | 1)
#define untag_scalar(s)    (((size_t)(s)) & (UINTPTR_MAX ^ 0x1))
// #define untag_scalar(s)    (((unsigned long)(s)) & 0xFFFFFFFE)
#else
#define tag_scalar(s)   (s)
#define untag_scalar(s) (s)
#endif
#define check_tag_scalar(s)  {if ((((size_t)(s)) | 1) == ((size_t)(s))) {die("Tagging.h:check tag scalar failed");} else {}}

/*----------------------------------------------------------------*
 * Converting reals.                                              *
 *----------------------------------------------------------------*/

/* extract double from storable value.
 * get_d may be used as l-value as well. */

#ifdef TAG_VALUES
#define get_d(s)     (* (double *)(((size_t *)s)+1))
#define set_dtag(d)  (* (size_t *)(d) = val_tag_real)
#define convertRealToC(mlReal)  (get_d((mlReal)))
#define convertRealToML(cReal, mlReal) {get_d((mlReal)) = (cReal);\
					set_dtag((mlReal));}
#ifdef PROFILING
#define allocRealProf(realRho, realPtr, pPoint) {\
  realPtr = (uintptr_t *) alloc(realRho, 3+sizeObjectDesc);\
  ((ObjectDesc *) realPtr)->atId = pPoint; \
  ((ObjectDesc *) realPtr)->size = 2; /* Size is two words. */ \
  realPtr = (uintptr_t *)(((ObjectDesc *)realPtr)+1); \
}
#else
#define allocReal(realRho, realPtr) {realPtr = (uintptr_t *) alloc(realRho,3);}
#endif
#else // NO TAGGING
#define get_d(s)     (* (double *)(((size_t *)s)))
#define set_dtag(d)  /* nothing */
#define convertRealToC(mlReal)  (get_d((mlReal)))
#define convertRealToML(cReal, mlReal) {get_d((mlReal)) = (cReal);}
#ifdef PROFILING
#define allocRealProf(realRho, realPtr, pPoint) {\
  realPtr = (uintptr_t *) alloc(realRho, 2+sizeObjectDesc);\
  ((ObjectDesc *) realPtr)->atId = pPoint; \
  ((ObjectDesc *) realPtr)->size = 2; /* Size is two words. */ \
  realPtr = (uintptr_t *)(((ObjectDesc *)realPtr)+1); \
}
#else
#define allocReal(realRho, realPtr) {realPtr = (uintptr_t *) alloc(realRho,2);}
#endif
#endif

/*----------------------------------------------------------------*
 * Converting booleans.                                           *
 *----------------------------------------------------------------*/
#define convertBoolToC(i)  ((i) >> 1)
#define convertBoolToML(i) (((i) << 1) + 1)

/*----------------------------------------------------------------*
 * Converting units.                                              *
 *----------------------------------------------------------------*/
#define convertUnitToC(u)
#define convertUnitToML(u) (mlUNIT)

/*----------------------------------------------------------------*
 * Converting records.                                            *
 *   mlTagRecordML: Store tag in record at address recAddr.       *
 *   storeElemRecordML: mem[recAddr+offset]=mlVal, where mlVal    *
 *                      has to be the value in ML representation. *
 *                      First elem in record has offset 0.        *
 *----------------------------------------------------------------*/

#ifdef TAG_VALUES
#define mkTagRecordML(recAddr, size)              (*((size_t *)recAddr) = val_tag_record(size))
#define mkScalarTagRecordML(recAddr,size)         (*((size_t *)recAddr) = val_tag_scalar_record(size))
#define elemRecordML(recAddr, offset)             (*((size_t *)(recAddr)+(offset+1)))
#define storeElemRecordML(recAddr, offset, mlVal) (*(((size_t *)recAddr)+(offset+1))=(mlVal))
#define allocRecordML(rAddr, size, recAddr)       {recAddr= (size_t *) alloc(rAddr, size+1);\
        				           mkTagRecordML(recAddr, size);}
#ifdef TAG_FREE_PAIRS
#define allocPairML(rAddr, recAddr)               (recAddr = (size_t *) alloc(rAddr,2) - 1)
#define mkTagPairML(recAddr)
#define allocTripleML(rAddr, recAddr)               (recAddr = (size_t *) alloc(rAddr,3) - 1)
#define mkTagTripleML(recAddr)
#else
#define allocPairML(rAddr, recAddr)               allocRecordML(rAddr, 2, recAddr)
#define mkTagPairML(recAddr)                      (*((uintptr_t *)recAddr) = val_tag_record(2))
#define allocTripleML(rAddr, recAddr)               allocRecordML(rAddr, 3, recAddr)
#define mkTagTripleML(recAddr)                      (*((uintptr_t *)recAddr) = val_tag_record(3))
#endif

#define first(x)   (*((size_t *)(x)+1))
#define second(x)  (*((size_t *)(x)+2))
#define third(x)   (*((size_t *)(x)+3))

#ifdef PROFILING
#define allocRecordMLProf(rAddr, ssize, recAddr, pPoint) { \
   recAddr = (uintptr_t *) alloc(rAddr, ssize+1+sizeObjectDesc); \
   ((ObjectDesc *) recAddr)->atId = pPoint; \
   ((ObjectDesc *) recAddr)->size = ssize+1; \
   recAddr = (uintptr_t *)(((ObjectDesc *)recAddr)+1); \
   mkTagRecordML(recAddr, ssize); \
}
#ifdef TAG_FREE_PAIRS
#define allocPairMLProf(rAddr, recAddr, pPoint) { \
   recAddr = (uintptr_t *) alloc(rAddr, 2+sizeObjectDesc); \
   ((ObjectDesc *) recAddr)->atId = pPoint; \
   ((ObjectDesc *) recAddr)->size = 2; \
   recAddr = ((uintptr_t *)(((ObjectDesc *)recAddr)+1)) - 1; \
}
#define allocTripleMLProf(rAddr, recAddr, pPoint) { \
   recAddr = (uintptr_t *) alloc(rAddr, 3+sizeObjectDesc); \
   ((ObjectDesc *) recAddr)->atId = pPoint; \
   ((ObjectDesc *) recAddr)->size = 3; \
   recAddr = ((uintptr_t *)(((ObjectDesc *)recAddr)+1)) - 1; \
}
#else
#define allocPairMLProf(rAddr, recAddr, pPoint) { \
   recAddr = (uintptr_t *) alloc(rAddr, 3+sizeObjectDesc); \
   ((ObjectDesc *) recAddr)->atId = pPoint; \
   ((ObjectDesc *) recAddr)->size = 3; \
   recAddr = (uintptr_t *)(((ObjectDesc *)recAddr)+1); \
   mkTagRecordML(recAddr, 2); \
}
#define allocTripleMLProf(rAddr, recAddr, pPoint) { \
   recAddr = (uintptr_t *) alloc(rAddr, 4+sizeObjectDesc); \
   ((ObjectDesc *) recAddr)->atId = pPoint; \
   ((ObjectDesc *) recAddr)->size = 4; \
   recAddr = (uintptr_t *)(((ObjectDesc *)recAddr)+1); \
   mkTagRecordML(recAddr, 3); \
}
#endif
#endif /*PROFILING*/

#else /* don't tag values */
#define mkTagRecordML(recAddr, size)
#define mkScalarTagRecordML(recAddr,size)
#define mkTagPairML(recAddr)
#define mkTagTripleML(recAddr)
#define elemRecordML(recAddr, offset) (*((size_t *)(recAddr)+(offset)))
#define storeElemRecordML(recAddr, offset, mlVal) (*(((size_t *)recAddr)+(offset))=(mlVal))
#define allocRecordML(rAddr, size, recAddr) {recAddr=(size_t *) alloc(rAddr, size);}
#define allocPairML(rAddr, recAddr) {recAddr= (uintptr_t *) alloc(rAddr, 2);}
#define allocTripleML(rAddr, recAddr) {recAddr= (uintptr_t *) alloc(rAddr, 3);}
#define first(x)   (*(size_t *)(x))
#define second(x)  (*((size_t *)(x)+1))
#define third(x)   (*((size_t *)(x)+2))

#ifdef PROFILING
#define allocRecordMLProf(rAddr, ssize, recAddr, pPoint) { \
   recAddr = (uintptr_t *) alloc(rAddr, ssize+sizeObjectDesc); \
   ((ObjectDesc *) recAddr)->atId = pPoint; \
   ((ObjectDesc *) recAddr)->size = ssize; \
   recAddr = (uintptr_t *)(((ObjectDesc *)recAddr)+1); \
}
#define allocPairMLProf(rAddr, recAddr, pPoint) { \
   recAddr = (uintptr_t *) alloc(rAddr, 2+sizeObjectDesc); \
   ((ObjectDesc *) recAddr)->atId = pPoint; \
   ((ObjectDesc *) recAddr)->size = 2; \
   recAddr = (uintptr_t *)(((ObjectDesc *)recAddr)+1); \
}
#define allocTripleMLProf(rAddr, recAddr, pPoint) { \
   recAddr = (uintptr_t *) alloc(rAddr, 3+sizeObjectDesc); \
   ((ObjectDesc *) recAddr)->atId = pPoint; \
   ((ObjectDesc *) recAddr)->size = 3; \
   recAddr = (uintptr_t *)(((ObjectDesc *)recAddr)+1); \
}
#endif /*PROFILING*/

#endif /*NO TAG_VALUES*/

#ifdef PROFILING
#define REG_POLY_FUN_HDR(name, ...)  name ## Prof(__VA_ARGS__, size_t pPoint)
#define REG_POLY_CALL(name, ...)     name ## Prof(__VA_ARGS__, pPoint)
#else
#define REG_POLY_FUN_HDR(name, ...)  name(__VA_ARGS__)
#define REG_POLY_CALL(name, ...) name(__VA_ARGS__)
#endif

#endif /*__TAGGING_H*/
