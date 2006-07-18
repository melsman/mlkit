/*----------------------------------------------------------------*
 *                        Math                                    *
 *----------------------------------------------------------------*/
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "Math.h"
#include "Tagging.h"
#include "Exception.h"

/*
static unsigned int 
max(unsigned int a, unsigned int b) 
{
  return (a<b)?b:a;
}*/

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
__div_int31(ssize_t x, ssize_t y, uintptr_t exn)                   /* ML */
{
  if (y == 1) 
    { 
      raise_exn(exn);
      return 0;                         // never reached
    }
  if ( y == -1 && x == -2147483647 )    // -2147483647 = 2 * Int31.minInt + 1
    {   
      raise_exn((uintptr_t)&exn_OVERFLOW);
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
__div_int32ub(ssize_t x, ssize_t y, uintptr_t exn)                 /* ML */
{
  if (y == 0) 
    {
      raise_exn(exn);
      return 0;                                // never reached
    }
  if ( y == -1 && x == (-2147483647 - 1) ) 
    {
      raise_exn((uintptr_t)&exn_OVERFLOW);
      return 0;                                // never reached
    }
  if (x < 0 && y > 0)
    return ((x + 1) / y) - 1;
  else if (x > 0 && y < 0)
    return ((x - 1) / y) - 1;
  else return x / y;
}

size_t 
__div_word32ub(size_t x, size_t y, uintptr_t exn)          /* ML */
{
  if ( y == 0 ) 
    {
      raise_exn(exn);
      return 0;                               // never reached
    } 
  return (x / y);
}

size_t 
__div_word31(size_t x, size_t y, uintptr_t exn)            /* ML */
{
  size_t xC = i31_to_i32ub(x);
  size_t yC = i31_to_i32ub(y);

  if ( yC == 0 ) 
    {
      raise_exn(exn);
      return 0;                               // never reached
    }
  return i32ub_to_i31(xC / yC);
}

ssize_t 
__mod_int31(ssize_t xML, ssize_t yML, uintptr_t exn) 
{
  if ( yML == 1 ) 
    {
      raise_exn(exn);
      return 0;                               // never reached
    }
  if ((xML-1)%(yML-1) == 0 || (xML>1 && yML>1) || (xML<1 && yML<1))
    return ((xML-1)%(yML-1))+1;
  else
    return ((xML-1)%(yML-1))+yML;
}

ssize_t 
__mod_int32ub(ssize_t x, ssize_t y, uintptr_t exn) 
{
  if ( y == 0 ) 
    {
      raise_exn(exn);
      return 0;                               // never reached
    }
  if ( (x > 0 && y > 0) || (x < 0 && y < 0) || (x % y == 0) ) 
    {
      return x % y;
    }
  return (x % y) + y;
}

size_t 
__mod_word32ub(size_t x, size_t y, uintptr_t exn) 
{
  if ( y == 0 ) 
    {
      raise_exn(exn);
      return 0;                              // never reached
    }
  return (x % y);
}

size_t 
__mod_word31(size_t x, size_t y, uintptr_t exn) 
{
  size_t xC = i31_to_i32ub(x);
  size_t yC = i31_to_i32ub(y);

  if ( yC == 0 ) 
    {
      raise_exn(exn);
      return 0;                              // never reached
    }
  return i32ub_to_i31(xC % yC);
}

ssize_t 
__quot_int32ub(ssize_t xML, ssize_t yML) 
{
  return xML/yML;
}

ssize_t 
__quot_int31(ssize_t xML, ssize_t yML) 
{
  ssize_t xC,yC;

  xC = i31_to_i32ub(xML);
  yC = i31_to_i32ub(yML);
  return i32ub_to_i31(xC / yC);
}

ssize_t 
__rem_int32ub(ssize_t xML, ssize_t yML) 
{
  return xML % yML;
}

ssize_t 
__rem_int31(ssize_t xML, ssize_t yML) 
{
  ssize_t xC,yC;

  xC = i31_to_i32ub(xML);
  yC = i31_to_i32ub(yML);

  return i32ub_to_i31(xC % yC);
}

#ifdef TAG_VALUES

size_t* 
__div_int32b(size_t* b, size_t* x, size_t* y, uintptr_t exn) 
{
  get_i32b(b) = __div_int32ub(get_i32b(x), get_i32b(y), exn);
  set_i32b_tag(b);
  return b;
}

size_t* 
__div_word32b(size_t* b, size_t* x, size_t* y, uintptr_t exn) 
{
  get_i32b(b) = __div_word32ub(get_i32b(x), get_i32b(y), exn);
  set_i32b_tag(b);
  return b;
}

size_t* 
__mod_int32b(size_t* b, size_t* x, size_t* y, uintptr_t exn) 
{
  get_i32b(b) = __mod_int32ub(get_i32b(x), get_i32b(y), exn);
  set_i32b_tag(b);
  return b;
}

size_t* 
__mod_word32b(size_t* b, size_t* x, size_t* y, uintptr_t exn) 
{
  get_i32b(b) = __mod_word32ub(get_i32b(x), get_i32b(y), exn);
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

#endif /*TAG_VALUES*/

ssize_t 
realInt(ssize_t d, ssize_t x) 
{
  debug(printf("[realInt: d = %x, x = %d\n", d, x));
  get_d(d) = (double) (convertIntToC(x));
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
floorFloat(ssize_t f) 
{ 
  double r;
  ssize_t i;

  r = get_d(f);
  if( r >= 0.0 ) 
    { 
      if ( r >= (Max_Int_d + 1.0) ) 
	{
	  raise_exn((uintptr_t)&exn_OVERFLOW);
	}
      return (convertIntToML((ssize_t) r));
    }
  if( r < Min_Int_d ) 
    {
      raise_exn((uintptr_t)&exn_OVERFLOW);
    }
  i = (ssize_t) r;
  if( r < ((double) i) ) 
    {
      i -= 1;
    }
  return convertIntToML(i);
}

ssize_t 
truncFloat(ssize_t f) 
{ 
  double r;

  r = get_d(f);
  if ((r >= (Max_Int_d + 1.0)) || (r <= (Min_Int_d - 1.0))) 
    { 
      raise_exn((uintptr_t)&exn_OVERFLOW);
    }
  return convertIntToML((ssize_t)r);
}

ssize_t 
ceilFloat(ssize_t f) 
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
  raise_exn((uintptr_t)&exn_OVERFLOW);
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
  if( countChar('.', buf) == 0 && countChar('E', buf) == 0 ) 
    {
      strcat(buf, ".0");
    }
  return REG_POLY_CALL(convertStringToML, rAddr,buf);
}

String
REG_POLY_FUN_HDR(generalStringOfFloat, Region rAddr, String format, size_t f) 
{
  char result_buf[512];

  /* Unfortunately there seems to be no way to ensure that this does not
   * crash by overflowing the result_buffer (e.g. when specifying a huge 
   * number of decimal digits in the fixed-point format): 
   */
  sprintf(result_buf, &(format->data), get_d(f));
  mkSMLMinus(result_buf);
  return REG_POLY_CALL(convertStringToML, rAddr, result_buf);
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
