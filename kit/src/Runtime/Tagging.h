/*----------------------------------------------------------------*
 *                    Tagging operations                          *
 *----------------------------------------------------------------*/
#ifndef TAGGING
#define TAGGING

#include "Flags.h"
#include "Region.h"

#define tag_kind(x)                 ((x) & 0x1F)         /* Least 5 significant bits    */
#define val_tag(x)                  (*(int *)x)
#define val_tag_kind(x)             ((*(int *)x) & 0x1F) /* Least 5 significant bits    */
#define is_const(x)                 (((int *)x) & 0x20)  /* Bit 6 is the constant bit   */
#define set_tag_const(x)            ((x) | 0x20)         /* Set bit 6, the constant bit */
#define clear_tag_const(x)          ((x) & 0xFFFFFFDF)   /* Clear bit 6                 */
#define val_tag_kind_const(x)       ((*(int *)x) & 0x3F) /* Least 6 significant bits    */

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
#ifdef PROFILING
#define is_large_string(x)  (is_string(x) && (((x)>>6) > (4*ALLOCATABLE_WORDS_IN_REGION_PAGE-12)))
#define is_large_table(x)   (is_table(x) && (((x)>>6) > (ALLOCATABLE_WORDS_IN_REGION_PAGE-3)))
#else
#define is_large_string(x)  (is_string(x) && (((x)>>6) > (4*ALLOCATABLE_WORDS_IN_REGION_PAGE-4)))
#define is_large_table(x)   (is_table(x) && (((x)>>6) > ALLOCATABLE_WORDS_IN_REGION_PAGE-1))
#endif
#define is_large_obj(x)     (is_large_string(x) || is_large_table(x))

#define val_tag_string(s)   (gen_string_tag((s),0,1))
#define get_string_size(s)  ((s) >> 6)
#define val_tag_con0(c_tag) (gen_string_tag((c_tag),0,2))
#define val_tag_con1(c_tag) (gen_string_tag((c_tag),0,3))
#define get_record_size(s)  ((int)(((unsigned int)(s)) >> 19))
#define get_record_skip(s)  (((int)(((unsigned int)(s)) >> 6)) & (0x1FFF))
#define val_tag_table(s)    (gen_string_tag((s),0,7))
#define get_table_size(s)   ((s) >> 6)

#ifdef TAG_VALUES
#define val_tag_real              (gen_record_tag(3,3,0,6))
#define val_tag_record(s)         (gen_record_tag((s),0,0,6))
#define val_tag_scalar_record(s)  (gen_record_tag((s),(s),0,6))
#define val_tag_ref               (gen_string_tag(0,0,5))
#define val_tag_clos(s,o)         (gen_record_tag((s),(o),0,6))
#define val_tag_sclos(s,o)        (gen_record_tag((s),(o),0,6))
#define val_tag_regvec(s)         (gen_record_tag((s),(s),0,6))
#define val_tag_exname            (gen_record_tag(2,2,0,6))
#define val_tag_excon0            (gen_record_tag(1,0,0,6))
#define val_tag_excon1            (gen_record_tag(2,0,0,6))
#define val_tag_i32b              (gen_record_tag(1,1,0,6))     /*hmmm; mael 2001-04-23 */
#endif

#define mlTRUE 3
#define mlFALSE 1
#define mlUNIT 1

#define TRUE 1
#define FALSE 0

/*----------------------------------------------------------------*
 * Converting integers.                                           * 
 *----------------------------------------------------------------*/

#define i32ub_to_i31(i)   (((i) << 1) + 1)
#define i31_to_i32ub(i)   ((i) >> 1)

#ifdef TAG_VALUES
#define convertIntToC(i)  ((i) >> 1)
#define convertIntToML(i) (((i) << 1) + 1)
#define get_i32b(b)       (* (((int *)b)+1))
#define set_i32b_tag(b)   (* (int *)(b) = val_tag_i32b)
#else
#define convertIntToC(i)  (i)
#define convertIntToML(i) (i)
#endif

/*----------------------------------------------------------------*
 * Tagging Scalars (used for non ML values)                       * 
 *----------------------------------------------------------------*/
#ifdef TAG_VALUES
#define tag_scalar(s)      (((unsigned int)(s)) | 1)
#define untag_scalar(s)    (((unsigned int)(s)) & 0xFFFFFFFE)
#else
#define tag_scalar(s)   (s)
#define untag_scalar(s) (s)
#endif
#define check_tag_scalar(s)  {if ((((unsigned int)(s)) | 1) == ((unsigned int)(s))) {die("Tagging.h:check tag scalar failed");} else {}}

/*----------------------------------------------------------------*
 * Converting reals.                                              * 
 *----------------------------------------------------------------*/

/* extract double from storable value. 
 * get_d may be used as l-value as well. */

#ifdef TAG_VALUES
#define get_d(s)     (* (double *)(((int *)s)+2))
#define set_dtag(d)  (* (int *)(d) = val_tag_real)
#define convertRealToC(mlReal)  (get_d((mlReal)))
#define convertRealToML(cReal, mlReal) {get_d((mlReal)) = (cReal);\
					set_dtag((mlReal));}
#else
#define get_d(s)     (* (double *)(((int *)s)))
#define set_dtag(d)  /* nothing */
#define convertRealToC(mlReal)  (get_d((mlReal)))
#define convertRealToML(cReal, mlReal) {get_d((mlReal)) = (cReal);}

#ifdef PROFILING
#define allocRealProf(realRho, realPtr, pPoint) {\
  realPtr = alloc(realRho, 2+sizeObjectDesc);\
  ((ObjectDesc *) realPtr)->atId = pPoint; \
  ((ObjectDesc *) realPtr)->size = 2; /* Size is two words. */ \
  realPtr = (int *)(((ObjectDesc *)realPtr)+1); \
}
#else 
#define allocReal(realRho, realPtr) {realPtr = alloc(realRho,2);}
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
#define mkTagRecordML(recAddr, size)              (*((int *)recAddr) = val_tag_record(size))
#define mkScalarTagRecordML(recAddr,size)         (*((int *)recAddr) = val_tag_scalar_record(size))
#define elemRecordML(recAddr, offset)             (*((int *)(recAddr)+(offset+1)))
#define storeElemRecordML(recAddr, offset, mlVal) (*(((int *)recAddr)+(offset+1))=(mlVal))
#define allocRecordML(rAddr, size, recAddr)       {recAddr=alloc(rAddr, size+1);\
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
#define mkScalarTagRecordML(recAddr,size)
#define elemRecordML(recAddr, offset) (*((int *)(recAddr)+(offset)))
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

#endif /*TAGGING*/
