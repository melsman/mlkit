/*----------------------------------------------------------------*
 *                        Math                                    *
 *----------------------------------------------------------------*/
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <float.h>
#include <fenv.h>
#include "Math.h"
#include "Tagging.h"
#include "Exception.h"

/*------------------------------------------------------------------------*
 *                         ML Integer Functions                           *
 *------------------------------------------------------------------------*/

ssize_t
max_fixed_int(ssize_t dummy)                             /* ML */
{
  return convertIntToML(Max_Int);
}

ssize_t
min_fixed_int(ssize_t dummy)                             /* ML */
{
  return convertIntToML(Min_Int);
}

ssize_t
precision(ssize_t dummy)                                 /* ML */
{
  return convertIntToML(val_precision);
}

ssize_t
__div_int31(Context ctx, ssize_t x0, ssize_t y0, uintptr_t exn)                   /* ML */
{
  int x = (int)x0;
  int y = (int)y0;
  if (y == 1)
    {
      raise_exn(ctx,exn);
      return 0;                         // never reached
    }
  if ( y == -1 && x == -2147483647 )    // -2147483647 = 2 * Int31.minInt + 1
    {
      raise_exn(ctx,(uintptr_t)&exn_OVERFLOW);
      return 0;                         // never reached
    }
  if (x == 1) return 1;
  if (x < 1 && y > 1)
    return (2*((x+1)/(y-1))-1);
  else if (x > 1 && y < 1)
    return (2*((x-3)/(y-1))-1);
  else return (2*((x-1)/(y-1))+1);
}

ssize_t
__div_int63(Context ctx, ssize_t x0, ssize_t y0, uintptr_t exn)                   /* ML */
{
  long int x = (long int)x0;
  long int y = (long int)y0;
  if (y == 1)
    {
      raise_exn(ctx,exn);
      return 0;                         // never reached
    }
  if ( y == -1 && x == ( 2 * (-4611686018427387904) + 1 ) )    // = 2 * Int63.minInt + 1
    {
      raise_exn(ctx,(uintptr_t)&exn_OVERFLOW);
      return 0;                         // never reached
    }
  if (x == 1) return 1;
  if (x < 1 && y > 1)
    return (2*((x+1)/(y-1))-1);
  else if (x > 1 && y < 1)
    return (2*((x-3)/(y-1))-1);
  else return (2*((x-1)/(y-1))+1);
}

ssize_t
__div_int32ub(Context ctx, ssize_t x0, ssize_t y0, uintptr_t exn)                 /* ML */
{
  int x = (int)x0;
  int y = (int)y0;
  if (y == 0)
    {
      raise_exn(ctx,exn);
      return 0;                                // never reached
    }
  if ( y == -1 && x == (-2147483647 - 1) )
    {
      raise_exn(ctx,(uintptr_t)&exn_OVERFLOW);
      return 0;                                // never reached
    }
  if (x < 0 && y > 0)
    return ((x + 1) / y) - 1;
  else if (x > 0 && y < 0)
    return ((x - 1) / y) - 1;
  else return x / y;
}

ssize_t
__div_int64ub(Context ctx, ssize_t x0, ssize_t y0, uintptr_t exn)                 /* ML */
{
  long int x = (long int)x0;
  long int y = (long int)y0;
  if (y == 0)
    {
      raise_exn(ctx,exn);
      return 0;                                // never reached
    }
  if ( y == -1 && x == (-9223372036854775807 - 1) )
    {
      raise_exn(ctx,(uintptr_t)&exn_OVERFLOW);
      return 0;                                // never reached
    }
  if (x < 0 && y > 0)
    return ((x + 1) / y) - 1;
  else if (x > 0 && y < 0)
    return ((x - 1) / y) - 1;
  else return x / y;
}

size_t
__div_word32ub(Context ctx, size_t x0, size_t y0, uintptr_t exn)          /* ML */
{
  unsigned int x = (unsigned int)x0;
  unsigned int y = (unsigned int)y0;
  if ( y == 0 )
    {
      raise_exn(ctx,exn);
      return 0;                               // never reached
    }
  return (x / y);
}

size_t
__div_word64ub(Context ctx, size_t x0, size_t y0, uintptr_t exn)          /* ML */
{
  unsigned long int x = (unsigned long int)x0;
  unsigned long int y = (unsigned long int)y0;
  if ( y == 0 )
    {
      raise_exn(ctx,exn);
      return 0;                               // never reached
    }
  return (x / y);
}

size_t
__div_word31(Context ctx, size_t x, size_t y, uintptr_t exn)            /* ML */
{
  unsigned int xC = i31_to_i32ub((unsigned int)x);
  unsigned int yC = i31_to_i32ub((unsigned int)y);

  if ( yC == 0 )
    {
      raise_exn(ctx,exn);
      return 0;                               // never reached
    }
  return i32ub_to_i31(xC / yC);
}

size_t
__div_word63(Context ctx, size_t x, size_t y, uintptr_t exn)            /* ML */
{
  unsigned long int xC = i63_to_i64ub((unsigned long int)x);
  unsigned long int yC = i63_to_i64ub((unsigned long int)y);

  if ( yC == 0 )
    {
      raise_exn(ctx,exn);
      return 0;                               // never reached
    }
  return i64ub_to_i63(xC / yC);
}

ssize_t
__mod_int31(Context ctx, ssize_t x0ML, ssize_t y0ML, uintptr_t exn)
{
  int xML = (int)x0ML;
  int yML = (int)y0ML;

  if ( yML == 1 )
    {
      raise_exn(ctx,exn);
      return 0;                               // never reached
    }
  if ((xML-1)%(yML-1) == 0 || (xML>1 && yML>1) || (xML<1 && yML<1))
    return ((xML-1)%(yML-1))+1;
  else
    return ((xML-1)%(yML-1))+yML;
}

ssize_t
__mod_int63(Context ctx, ssize_t x0ML, ssize_t y0ML, uintptr_t exn)
{
  long int xML = (long int)x0ML;
  long int yML = (long int)y0ML;

  if ( yML == 1 )
    {
      raise_exn(ctx,exn);
      return 0;                               // never reached
    }
  if ((xML-1)%(yML-1) == 0 || (xML>1 && yML>1) || (xML<1 && yML<1))
    return ((xML-1)%(yML-1))+1;
  else
    return ((xML-1)%(yML-1))+yML;
}

ssize_t
__mod_int32ub(Context ctx, ssize_t x0, ssize_t y0, uintptr_t exn)
{
  int x = (int)x0;
  int y = (int)y0;
  if ( y == 0 )
    {
      raise_exn(ctx,exn);
      return 0;                               // never reached
    }
  if ( (x > 0 && y > 0) || (x < 0 && y < 0) || (x % y == 0) )
    {
      return x % y;
    }
  return (x % y) + y;
}

ssize_t
__mod_int64ub(Context ctx, ssize_t x0, ssize_t y0, uintptr_t exn)
{
  long int x = (long int)x0;
  long int y = (long int)y0;
  if ( y == 0 )
    {
      raise_exn(ctx,exn);
      return 0;                               // never reached
    }
  if ( (x > 0 && y > 0) || (x < 0 && y < 0) || (x % y == 0) )
    {
      return x % y;
    }
  return (x % y) + y;
}

size_t
__mod_word32ub(Context ctx, size_t x0, size_t y0, uintptr_t exn)
{
  unsigned int x = (unsigned int)x0;
  unsigned int y = (unsigned int)y0;
  if ( y == 0 )
    {
      raise_exn(ctx,exn);
      return 0;                              // never reached
    }
  return (x % y);
}

size_t
__mod_word64ub(Context ctx, size_t x0, size_t y0, uintptr_t exn)
{
  unsigned long int x = (unsigned long int)x0;
  unsigned long int y = (unsigned long int)y0;
  if ( y == 0 )
    {
      raise_exn(ctx,exn);
      return 0;                              // never reached
    }
  return (x % y);
}

size_t
__mod_word31(Context ctx, size_t x, size_t y, uintptr_t exn)
{
  unsigned int xC = i31_to_i32ub((unsigned int)x);
  unsigned int yC = i31_to_i32ub((unsigned int)y);

  if ( yC == 0 )
    {
      raise_exn(ctx,exn);
      return 0;                              // never reached
    }
  return i32ub_to_i31(xC % yC);
}

size_t
__mod_word63(Context ctx, size_t x, size_t y, uintptr_t exn)
{
  unsigned long int xC = i63_to_i64ub((unsigned long int)x);
  unsigned long int yC = i63_to_i64ub((unsigned long int)y);

  if ( yC == 0 )
    {
      raise_exn(ctx,exn);
      return 0;                              // never reached
    }
  return i64ub_to_i63(xC % yC);
}

ssize_t
__quot_int32ub(ssize_t xML, ssize_t yML)
{
  return ((int)xML)/((int)yML);
}

ssize_t
__quot_int64ub(ssize_t xML, ssize_t yML)
{
  return ((long int)xML)/((long int)yML);
}

ssize_t
__quot_int31(ssize_t xML, ssize_t yML)
{
  int xC,yC;

  xC = i31_to_i32ub((int)xML);
  yC = i31_to_i32ub((int)yML);
  return i32ub_to_i31(xC / yC);
}

ssize_t
__quot_int63(ssize_t xML, ssize_t yML)
{
  long int xC,yC;

  xC = i63_to_i64ub((long int)xML);
  yC = i63_to_i64ub((long int)yML);
  return i64ub_to_i63(xC / yC);
}

ssize_t
__rem_int32ub(ssize_t xML, ssize_t yML)
{
  return ((int)xML) % ((int)yML);
}

ssize_t
__rem_int64ub(ssize_t xML, ssize_t yML)
{
  return ((long int)xML) % ((long int)yML);
}

ssize_t
__rem_int31(ssize_t xML, ssize_t yML)
{
  int xC,yC;

  xC = i31_to_i32ub((int)xML);
  yC = i31_to_i32ub((int)yML);

  return i32ub_to_i31(xC % yC);
}

ssize_t
__rem_int63(ssize_t xML, ssize_t yML)
{
  long int xC,yC;

  xC = i63_to_i64ub((long int)xML);
  yC = i63_to_i64ub((long int)yML);

  return i64ub_to_i63(xC % yC);
}

#ifdef TAG_VALUES

size_t*
__div_int32b(size_t* b, Context ctx, size_t* x, size_t* y, uintptr_t exn)
{
  get_i32b(b) = __div_int32ub(ctx, get_i32b(x), get_i32b(y), exn);
  set_i32b_tag(b);
  return b;
}

size_t*
__div_word32b(size_t* b, Context ctx, size_t* x, size_t* y, uintptr_t exn)
{
  get_i32b(b) = __div_word32ub(ctx, get_i32b(x), get_i32b(y), exn);
  set_i32b_tag(b);
  return b;
}

size_t*
__mod_int32b(size_t* b, Context ctx, size_t* x, size_t* y, uintptr_t exn)
{
  get_i32b(b) = __mod_int32ub(ctx, get_i32b(x), get_i32b(y), exn);
  set_i32b_tag(b);
  return b;
}

size_t*
__mod_word32b(size_t* b, Context ctx, size_t* x, size_t* y, uintptr_t exn)
{
  get_i32b(b) = __mod_word32ub(ctx, get_i32b(x), get_i32b(y), exn);
  set_i32b_tag(b);
  return b;
}

// quot need not check for y being 0; this is checked for in Int32
size_t*
__quot_int32b(size_t* b, size_t* x, size_t* y)
{
  get_i32b(b) = __quot_int32ub(get_i32b(x), get_i32b(y));
  set_i32b_tag(b);
  return b;
}

// rem need not check for y being 0; this is checked for in Int32
size_t*
__rem_int32b(size_t* b, size_t* x, size_t* y)
{
  get_i32b(b) = __rem_int32ub(get_i32b(x), get_i32b(y));
  set_i32b_tag(b);
  return b;
}

size_t*
__div_int64b(size_t* b, Context ctx, size_t* x, size_t* y, uintptr_t exn)
{
  get_i64b(b) = __div_int64ub(ctx, get_i64b(x), get_i64b(y), exn);
  set_i64b_tag(b);
  return b;
}

size_t*
__div_word64b(size_t* b, Context ctx, size_t* x, size_t* y, uintptr_t exn)
{
  get_i64b(b) = __div_word64ub(ctx, get_i64b(x), get_i64b(y), exn);
  set_i64b_tag(b);
  return b;
}

size_t*
__mod_int64b(size_t* b, Context ctx, size_t* x, size_t* y, uintptr_t exn)
{
  get_i64b(b) = __mod_int64ub(ctx, get_i64b(x), get_i64b(y), exn);
  set_i64b_tag(b);
  return b;
}

size_t*
__mod_word64b(size_t* b, Context ctx, size_t* x, size_t* y, uintptr_t exn)
{
  get_i64b(b) = __mod_word64ub(ctx, get_i64b(x), get_i64b(y), exn);
  set_i64b_tag(b);
  return b;
}

// quot need not check for y being 0; this is checked for in Int64
size_t*
__quot_int64b(size_t* b, size_t* x, size_t* y)
{
  get_i64b(b) = __quot_int64ub(get_i64b(x), get_i64b(y));
  set_i64b_tag(b);
  return b;
}

// rem need not check for y being 0; this is checked for in Int64
size_t*
__rem_int64b(size_t* b, size_t* x, size_t* y)
{
  get_i64b(b) = __rem_int64ub(get_i64b(x), get_i64b(y));
  set_i64b_tag(b);
  return b;
}

#endif /*TAG_VALUES*/

ssize_t
realInt(ssize_t d, ssize_t x)
{
  debug(printf("[realInt: d = %zu, x = %zu\n", d, x));
  get_d(d) = (double) (convertIntToC((long int)x));
  set_dtag(d);
  debug(printf("]\n"));
  return d;
}

/*----------------------------------------------------------------------*
 *                      Floating point operations                       *
 *----------------------------------------------------------------------*/

ssize_t
divFloat(ssize_t d, ssize_t x, ssize_t y)
{
  get_d(d) = get_d(x) / get_d(y);
  set_dtag(d);
  return d;
}

ssize_t
remFloat(ssize_t d, ssize_t x, ssize_t y)
{
  get_d(d) = fmod(get_d(x), get_d(y));
  set_dtag(d);
  return d;
}

ssize_t
realFloor(ssize_t d, ssize_t x)
{
  get_d(d) = floor(get_d(x));
  set_dtag(d);
  return d;
}

ssize_t
realCeil(ssize_t d, ssize_t x)
{
  get_d(d) = ceil(get_d(x));
  set_dtag(d);
  return d;
}

ssize_t
realTrunc(ssize_t d, ssize_t x)
{
  get_d(d) = trunc(get_d(x));
  set_dtag(d);
  return d;
}

ssize_t
realRound(ssize_t d, ssize_t x)
{
  get_d(d) = round(get_d(x));
  set_dtag(d);
  return d;
}

long int
floorFloat(Context ctx, ssize_t f)
{
  double r;
  long int i;

  r = get_d(f);
  if( r >= 0.0 )
    {
      if ( r >= (Max_Int_d + 1.0) )
	{
	  raise_exn(ctx,(uintptr_t)&exn_OVERFLOW);
	}
      return (convertIntToML((long int) r));
    }
  if( r < Min_Int_d )
    {
      raise_exn(ctx,(uintptr_t)&exn_OVERFLOW);
    }
  i = (long int) r;
  if( r < ((double) i) )
    {
      i -= 1L;
    }
  return convertIntToML(i);
}

ssize_t
truncFloat(Context ctx, ssize_t f)
{
  double r;

  r = get_d(f);
  if ((r >= (Max_Int_d + 1.0)) || (r <= (Min_Int_d - 1.0)))
    {
      raise_exn(ctx,(uintptr_t)&exn_OVERFLOW);
    }
  return convertIntToML((ssize_t)r);
}

ssize_t
ceilFloat(Context ctx, ssize_t f)
{
  double arg;
  ssize_t i;

  arg = get_d(f);

  if( arg >= 0.0 )
    {
      if( arg > Max_Int_d ) goto raise_ceil;
      i = (ssize_t) arg;
      if( arg > ((double) i) ) i += 1;
    }
  else
    {
      if( arg <= (Min_Int_d - 1.0) ) goto raise_ceil;
      i = (ssize_t) arg;
    }
  return convertIntToML(i);

 raise_ceil:
  raise_exn(ctx,(uintptr_t)&exn_OVERFLOW);
  return 0;                          // never reached
}

ssize_t
sqrtFloat(ssize_t d, ssize_t s)
{
  get_d(d) = sqrt(get_d(s));
  set_dtag(d);
  return d;
}

ssize_t
sinFloat(ssize_t d, ssize_t s)
{
  get_d(d) = sin(get_d(s));
  set_dtag(d);
  return d;
}

ssize_t
cosFloat(ssize_t d, ssize_t s)
{
  get_d(d) = cos(get_d(s));
  set_dtag(d);
  return d;
}

ssize_t
atanFloat (ssize_t d, ssize_t s)
{
  get_d (d) = atan (get_d (s));
  set_dtag(d);
  return d;
}

ssize_t
asinFloat (ssize_t d, ssize_t s)
{
  get_d (d) = asin (get_d (s));
  set_dtag(d);
  return d;
}

ssize_t
acosFloat (ssize_t d, ssize_t s)
{
  get_d (d) = acos (get_d (s));
  set_dtag(d);
  return d;
}

ssize_t
atan2Float (ssize_t d, ssize_t y, ssize_t x)
{
  get_d (d) = atan2 (get_d (y), get_d (x));
  set_dtag(d);
  return d;
}

ssize_t expFloat(ssize_t d, ssize_t s)
{
  get_d(d) = exp(get_d(s));
  set_dtag(d);
  return d;
}

ssize_t
powFloat (ssize_t d, ssize_t x, ssize_t y)
{
  get_d (d) = pow (get_d (x), get_d (y));
  set_dtag(d);
  return d;
}

ssize_t
lnFloat(ssize_t d, ssize_t s)
{
  get_d(d) = log(get_d(s));
  set_dtag(d);
  return d;
}

ssize_t
sinhFloat(ssize_t d, ssize_t s)
{
  get_d(d) = sinh(get_d(s));
  set_dtag(d);
  return d;
}

ssize_t
coshFloat(ssize_t d, ssize_t s)
{
  get_d(d) = cosh(get_d(s));
  set_dtag(d);
  return d;
}

ssize_t
tanhFloat(ssize_t d, ssize_t s)
{
  get_d(d) = tanh(get_d(s));
  set_dtag(d);
  return d;
}

ssize_t
isnanFloat(ssize_t s)
{
  if (isnan(get_d(s)))
    {
      return mlTRUE;
    }
  return mlFALSE;
}

ssize_t
signbitFloat(ssize_t s)
{
  if (signbit(get_d(s)))
    {
      return mlTRUE;
    }
  return mlFALSE;
}

ssize_t
isnormalFloat(ssize_t s)
{
  if (isnormal(get_d(s)))
    {
      return mlTRUE;
    }
  return mlFALSE;
}

ssize_t
copysignFloat (ssize_t d, ssize_t y, ssize_t x)
{
  get_d (d) = copysign (get_d (y), get_d (x));
  set_dtag(d);
  return d;
}

ssize_t
ldexpFloat (ssize_t d, ssize_t x, ssize_t e)
{
  long int e2 = convertIntToC((long int)e);
  get_d (d) = ldexp (get_d (x), e2);
  set_dtag(d);
  return d;
}

ssize_t
frexpFloat (ssize_t p, ssize_t d, ssize_t x)
{
  int e = 0;
  get_d (d) = frexp (get_d (x), &e);
  set_dtag(d);
  first(p) = d;
  second(p) = convertIntToML((long int)e);
  return p;
}

ssize_t
posInfFloat(ssize_t d)
{
  get_d(d) = HUGE_VAL;
  set_dtag(d);
  return d;
}

ssize_t
negInfFloat(ssize_t d)
{
  get_d(d) = -HUGE_VAL;
  set_dtag(d);
  return d;
}

ssize_t
maxFiniteFloat(ssize_t d)
{
  get_d(d) = DBL_MAX;
  set_dtag(d);
  return d;
}

ssize_t
nextafterFloat (ssize_t d, ssize_t x, ssize_t y)
{
  get_d (d) = nextafter (get_d (x), get_d (y));
  set_dtag(d);
  return d;
}

ssize_t
splitFloat (ssize_t p, ssize_t w, ssize_t f, ssize_t r)
{
  double whole;
  get_d (f) = modf (get_d (r), &whole);
  get_d (w) = whole;
  set_dtag(w);
  set_dtag(f);
  first(p) = w;
  second(p) = f;
  return p;
}

// IEEE rounding modes
// 0:TONEAREST, 1: DOWNWARD, 2: UPWARD, 3: ZERO
void
floatSetRoundingMode (ssize_t m) {
  long int rm = convertIntToC((long int)m);
  if ( rm == 0 ) {
    fesetround(FE_TONEAREST);
  } else if ( rm == 1 ) {
    fesetround(FE_DOWNWARD);
  } else if ( rm == 2 ) {
    fesetround(FE_UPWARD);
  } else if ( rm == 3 ) {
    fesetround(FE_TOWARDZERO);
  } else {
    printf("ERROR floatSetRoundingMode: %ld\n", rm);
    exit(1);
  }
}

ssize_t
floatGetRoundingMode(void) {
  long int m = 0;
  int rm = fegetround();
  if ( rm == FE_TONEAREST ) {
    m = 0;
  } else if ( rm == FE_DOWNWARD) {
    m = 1;
  } else if ( rm == FE_UPWARD) {
    m = 2;
  } else if ( rm == FE_TOWARDZERO) {
    m = 3;
  } else {
    printf("ERROR floatGetRoundingMode: %d\n", rm);
    exit(1);
  }
  return convertIntToML(m);
}

// countChar: count the number of times the character `c'
//     occurs in the string `s'.
static ssize_t countChar(ssize_t c, char * s) {
  char *p;
  ssize_t count;

  count = 0;
  for( p=s; *p != '\0'; p++ )
    {
      if( *p == c ) count++;
    }
  return count;
}

// mkSMLMinus: remove all '+', and replace '-' and 'e'
//     with '~' and 'E', respectively.
static void mkSMLMinus(char * s) {
  char *p, *q;

  for( p = s, q = s; *p != '\0'; p++ ) {
    switch( *p ) {
    case '+': break;
    case '-': *q++ = '~'; break;
    case 'e': *q++ = 'E'; break;
    default: *q++ = *p;
    }
  }
  *q = '\0';
  return;
}

String
REG_POLY_FUN_HDR(stringOfFloat, Region rAddr, size_t arg)
{
  char buf[64];
  sprintf(buf, "%.12g", get_d(arg));
  mkSMLMinus(buf);
  if( countChar('.', buf) == 0 && countChar('E', buf) == 0 && countChar('n', buf) == 0)  // protect for nan and inf
    {
      strcat(buf, ".0");
    }
  if ( strcmp (buf, "~nan") == 0 ) {
    return REG_POLY_CALL(convertStringToML,rAddr,"nan");
  } else {
    return REG_POLY_CALL(convertStringToML,rAddr,buf);
  }
}

String
REG_POLY_FUN_HDR(generalStringOfFloat, Region rAddr, String format, size_t f)
{
  char buf[512];

  /* Unfortunately there seems to be no way to ensure that this does not
   * crash by overflowing the result_buffer (e.g. when specifying a huge
   * number of decimal digits in the fixed-point format):
   */
  sprintf(buf, &(format->data), get_d(f));
  mkSMLMinus(buf);
  if ( strcmp (buf, "~nan") == 0 ) {
    return REG_POLY_CALL(convertStringToML,rAddr,"nan");
  } else {
    return REG_POLY_CALL(convertStringToML,rAddr,buf);
  }
}

/* DEBUG */
void
printReal(size_t f)
{
  printf("Num: %5.2f\n",get_d(f));
  return;
}

String
REG_POLY_FUN_HDR(sml_real_to_bytes, Region rAddr, size_t f)
{
  size_t i;
  double a[1];
  char v[sizeof(double) + 1];
  a[0] = get_d(f);
  for (i = 0; i < sizeof(double) ; i++)
    v[i] = ((char*)a)[i];
  v[sizeof(double)] = 0;
  return REG_POLY_CALL(convertBinStringToML, rAddr, 8, v);
}

size_t
sml_bytes_to_real(size_t d, String s)
{
  double r;
  char* a = &(s->data);
  r = ((double*)a)[0];
  get_d(d) = r;
  set_dtag(d);
  return d;
}

/* A test function for testing auto */
uintptr_t
runtime_test0 (uintptr_t a1, uintptr_t a2, uintptr_t a3) {
  long int ret =
    1 * (long int)a1 +
    3 * (long int)a2 +
    5 * (long int)a3;
  return ret;              /* (1*1)+(3*2)+(5*3) ==> 22 */
}

/* A test function for testing multi-parameter passing (non-auto) */
uintptr_t
runtime_test1 (uintptr_t a1, uintptr_t a2, uintptr_t a3, uintptr_t a4, uintptr_t a5,
	       uintptr_t a6, uintptr_t a7, uintptr_t a8, uintptr_t a9, uintptr_t a10) {
  long int ret =
    2 * (long int)(convertIntToC(a1)) +
    3 * (long int)(convertIntToC(a2)) +
    5 * (long int)(convertIntToC(a3)) +
    7 * (long int)(convertIntToC(a4)) +
    11 * (long int)(convertIntToC(a5)) +
    17 * (long int)(convertIntToC(a6)) +
    19 * (long int)(convertIntToC(a7)) +
    23 * (long int)(convertIntToC(a8)) +
    29 * (long int)(convertIntToC(a9)) +
    31 * (long int)(convertIntToC(a10));
  return (uintptr_t)(convertIntToML(ret));
}

/* (2*1)+(3*2)+(5*3)+(7*4)+(11*5)+(17*6)+(19*7)+(23*8)+(29*9)+(31*10) ==> 1096 */
