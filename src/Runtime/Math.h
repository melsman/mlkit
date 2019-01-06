/*----------------------------------------------------------------*
 *                         Math                                   *
 *----------------------------------------------------------------*/

#ifndef __MATH_H
#define __MATH_H

#include <stdint.h>
#include <stdlib.h>
#include "Flags.h"
#include "Tagging.h"
#include "String.h"

/*----------------------------------------------------------------*
 *                       Integer operations.                      *
 *----------------------------------------------------------------*/

#ifdef TAG_VALUES
#define muliML(x,y,d)  ((d)=1+((x)>>1)*((y)-1))
#define addiML(x,y,d)  ((d)=(x)+(y)-1)
#define subiML(x,y,d)  ((d)=(x)-(y)+1)
#else
#define muliML(x,y,d)  ((d)=(x)*(y))
#define addiML(x,y,d)  ((d)=(x)+(y))
#define subiML(x,y,d)  ((d)=(x)-(y))
#endif

#define  minDefine(A,B) ((A<B)?A:B)

#ifdef TAG_VALUES
#define Max_Int 1073741823       /* remember [i] = 2 * i + 1 */
#define Min_Int -1073741824
#define Max_Int_d 1073741823.0
#define Min_Int_d -1073741824.0
#define val_precision 31
/*
#define Max_Int (INTPTR_MAX >> 1)
#define Min_Int (INTPTR_MIN >> 1)
#define Max_Int_d ((INTPTR_MAX >> 1) * 1.0)
#define Min_Int_d ((INTPTR_MIN >> 1) * 1.0)
#define val_precision (__WORDSIZE - 1)
*/
#else
#define Max_Int 2147483647
#define Min_Int (-2147483647-1)
#define Max_Int_d 2147483647.0
#define Min_Int_d -2147483648.0
#define val_precision 32
/*
#define Max_Int INTPTR_MAX
#define Min_Int INTPTR_MIN
#define Max_Int_d (INTPTR_MAX * 1.0)
#define Min_Int_d (INTPTR_MIN * 1.0)
#define val_precision (__WORDSIZE)
*/
#endif

/*----------------------------------------------------------------*
 *        Prototypes for external and internal functions.         *
 *----------------------------------------------------------------*/
ssize_t max_fixed_int(ssize_t dummy);
ssize_t min_fixed_int(ssize_t dummy);
ssize_t precision(ssize_t dummy);
ssize_t __div_int32ub(ssize_t x, ssize_t y, uintptr_t exn);
ssize_t __div_int31(ssize_t x, ssize_t y, uintptr_t exn);
size_t __div_word32ub(size_t x, size_t y, uintptr_t exn);
size_t __div_word31(size_t x, size_t y, uintptr_t exn);
ssize_t __mod_int32ub(ssize_t x, ssize_t y, uintptr_t exn);
ssize_t __mod_int31(ssize_t x, ssize_t y, uintptr_t exn);
size_t __mod_word32ub(size_t x, size_t y, uintptr_t exn);
size_t __mod_word31(size_t x, size_t y, uintptr_t exn);
ssize_t __quot_int32ub(ssize_t x, ssize_t y);
ssize_t __quot_int31(ssize_t x, ssize_t y);
ssize_t __rem_int32ub(ssize_t x, ssize_t y);
ssize_t __rem_int31(ssize_t x, ssize_t y);
ssize_t realInt(ssize_t d, ssize_t x);
ssize_t floorFloat(ssize_t f);
ssize_t ceilFloat(ssize_t f);
ssize_t roundFloat(ssize_t f);
ssize_t truncFloat(ssize_t f);
ssize_t divFloat(ssize_t d, ssize_t x, ssize_t y);

ssize_t sqrtFloat(ssize_t d, ssize_t s);
ssize_t sinFloat(ssize_t d, ssize_t s);
ssize_t cosFloat(ssize_t d, ssize_t s);
ssize_t atanFloat(ssize_t d, ssize_t s);
ssize_t asinFloat(ssize_t d, ssize_t s);
ssize_t acosFloat(ssize_t d, ssize_t s);
ssize_t atan2Float(ssize_t d, ssize_t y, ssize_t x);
ssize_t expFloat(ssize_t d, ssize_t s);
ssize_t powFloat(ssize_t d, ssize_t x, ssize_t y);
ssize_t lnFloat(ssize_t d, ssize_t s);
ssize_t sinhFloat(ssize_t d, ssize_t s);
ssize_t coshFloat(ssize_t d, ssize_t s);
ssize_t tanhFloat(ssize_t d, ssize_t s);
ssize_t isnanFloat(ssize_t s);
ssize_t posInfFloat(ssize_t d);
ssize_t negInfFloat(ssize_t d);

// #ifdef PROFILING
// String stringOfFloatProf(Region rAddr, long f, uintptr_t pPoint);
// String generalStringOfFloatProf(Region rAddr, String str, long f, long pPoint);
// #else
String REG_POLY_FUN_HDR(stringOfFloat,Region rAddr, size_t f);
String REG_POLY_FUN_HDR(generalStringOfFloat,Region rAddr, String str, size_t f);
// #endif

/* For basislib Math structure */
ssize_t sml_sqrt(ssize_t d, ssize_t s);

/* For basislib PackReal{Big,Little} structures */
//#ifdef PROFILING
//String sml_real_to_bytes(Region rAddr, long f, long pPoint);
//#else
String REG_POLY_FUN_HDR(sml_real_to_bytes,Region rAddr, size_t f);
//#endif
size_t sml_bytes_to_real(size_t d, String s);

void printReal(size_t f);

#ifdef TAG_VALUES
size_t* __div_int32b(size_t* b, size_t* x, size_t* y, uintptr_t exn);
size_t* __div_word32b(size_t* b, size_t* x, size_t* y, uintptr_t exn);
size_t* __mod_int32b(size_t* b, size_t* x, size_t* y, uintptr_t exn);
size_t* __mod_word32b(size_t* b, size_t* x, size_t* y, uintptr_t exn);
size_t* __quot_int32b(size_t* b, size_t* x, size_t* y);
size_t* __rem_int32b(size_t* b, size_t* x, size_t* y);
#endif

#endif /*__MATH_H*/
