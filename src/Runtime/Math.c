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

int 
max_fixed_int(int dummy)                             /* ML */
{
  return convertIntToML(Max_Int);
}

int 
min_fixed_int(int dummy)                             /* ML */
{
  return convertIntToML(Min_Int);
}

int 
precision(int dummy)                                 /* ML */
{
  return convertIntToML(val_precision);
}

int 
__div_int31(int x, int y, int exn)                   /* ML */
{
  if (y == 1) 
    { 
      raise_exn(exn);
      return 0;                         // never reached
    }
  if ( y == -1 && x == -2147483647 )    // -2147483647 = 2 * Int31.minInt + 1
    {   
      raise_exn((int)&exn_OVERFLOW);
      return 0;                         // never reached
    }
  if (x == 1) return 1;
  if (x < 1 && y > 1)
    return (2*((x+1)/(y-1))-1);
  else if (x > 1 && y < 1)
    return (2*((x-3)/(y-1))-1);
  else return (2*((x-1)/(y-1))+1);
}

int 
__div_int32ub(int x, int y, int exn)                 /* ML */
{
  if (y == 0) 
    {
      raise_exn(exn);
      return 0;                                // never reached
    }
  if ( y == -1 && x == (-2147483647 - 1) ) 
    {
      raise_exn((int)&exn_OVERFLOW);
      return 0;                                // never reached
    }
  if (x < 0 && y > 0)
    return ((x + 1) / y) - 1;
  else if (x > 0 && y < 0)
    return ((x - 1) / y) - 1;
  else return x / y;
}

unsigned int 
__div_word32ub(unsigned int x, unsigned int y, int exn)          /* ML */
{
  if ( y == 0 ) 
    {
      raise_exn(exn);
      return 0;                               // never reached
    } 
  return (x / y);
}

unsigned int 
__div_word31(unsigned int x, unsigned int y, int exn)            /* ML */
{
  unsigned int xC = i31_to_i32ub(x);
  unsigned int yC = i31_to_i32ub(y);

  if ( yC == 0 ) 
    {
      raise_exn(exn);
      return 0;                               // never reached
    }
  return i32ub_to_i31(xC / yC);
}

int 
__mod_int31(int xML, int yML, int exn) 
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

int 
__mod_int32ub(int x, int y, int exn) 
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

unsigned int 
__mod_word32ub(unsigned int x, unsigned int y, int exn) 
{
  if ( y == 0 ) 
    {
      raise_exn(exn);
      return 0;                              // never reached
    }
  return (x % y);
}

unsigned int 
__mod_word31(unsigned int x, unsigned int y, int exn) 
{
  unsigned int xC = i31_to_i32ub(x);
  unsigned int yC = i31_to_i32ub(y);

  if ( yC == 0 ) 
    {
      raise_exn(exn);
      return 0;                              // never reached
    }
  return i32ub_to_i31(xC % yC);
}

int 
__quot_int32ub(int xML, int yML) 
{
  return xML/yML;
}

int 
__quot_int31(int xML, int yML) 
{
  int xC,yC;

  xC = i31_to_i32ub(xML);
  yC = i31_to_i32ub(yML);
  return i32ub_to_i31(xC / yC);
}

int 
__rem_int32ub(int xML, int yML) 
{
  return xML % yML;
}

int 
__rem_int31(int xML, int yML) 
{
  int xC,yC;

  xC = i31_to_i32ub(xML);
  yC = i31_to_i32ub(yML);

  return i32ub_to_i31(xC % yC);
}

#ifdef TAG_VALUES

unsigned int* 
__div_int32b(unsigned int* b, unsigned int* x, unsigned int* y, int exn) 
{
  get_i32b(b) = __div_int32ub(get_i32b(x), get_i32b(y), exn);
  set_i32b_tag(b);
  return b;
}

unsigned int* 
__div_word32b(unsigned int* b, unsigned int* x, unsigned int* y, int exn) 
{
  get_i32b(b) = __div_word32ub(get_i32b(x), get_i32b(y), exn);
  set_i32b_tag(b);
  return b;
}

unsigned int* 
__mod_int32b(unsigned int* b, unsigned int* x, unsigned int* y, int exn) 
{
  get_i32b(b) = __mod_int32ub(get_i32b(x), get_i32b(y), exn);
  set_i32b_tag(b);
  return b;
}

unsigned int* 
__mod_word32b(unsigned int* b, unsigned int* x, unsigned int* y, int exn) 
{
  get_i32b(b) = __mod_word32ub(get_i32b(x), get_i32b(y), exn);
  set_i32b_tag(b);
  return b;
}

// quot need not check for y being 0; this is checked for in Int32
unsigned int* 
__quot_int32b(unsigned int* b, unsigned int* x, unsigned int* y) 
{
  get_i32b(b) = __quot_int32ub(get_i32b(x), get_i32b(y));
  set_i32b_tag(b);
  return b;
}

// rem need not check for y being 0; this is checked for in Int32
unsigned int* 
__rem_int32b(unsigned int* b, unsigned int* x, unsigned int* y) 
{
  get_i32b(b) = __rem_int32ub(get_i32b(x), get_i32b(y));
  set_i32b_tag(b);
  return b;
}

#endif /*TAG_VALUES*/

int 
realInt(int d, int x) 
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

int 
divFloat(int d, int x, int y) 
{
  get_d(d) = get_d(x) / get_d(y);
  set_dtag(d);
  return d;
}

int 
floorFloat(int f) 
{ 
  double r;
  int i;

  r = get_d(f);
  if( r >= 0.0 ) 
    { 
      if ( r >= (Max_Int_d + 1.0) ) 
	{
	  raise_exn((int)&exn_OVERFLOW);
	}
      return (convertIntToML((int) r));
    }
  if( r < Min_Int_d ) 
    {
      raise_exn((int)&exn_OVERFLOW);
    }
  i = (int) r;
  if( r < ((double) i) ) 
    {
      i -= 1;
    }
  return convertIntToML(i);
}

int 
truncFloat(int f) 
{ 
  double r;

  r = get_d(f);
  if ((r >= (Max_Int_d + 1.0)) || (r <= (Min_Int_d - 1.0))) 
    { 
      raise_exn((int)&exn_OVERFLOW);
    }
  return convertIntToML((int)r);
}

int 
ceilFloat(int f) 
{ 
  double arg;
  int i;

  arg = get_d(f);

  if( arg >= 0.0 ) 
    { 
      if( arg > Max_Int_d ) goto raise_ceil;
      i = (int) arg;
      if( arg > ((double) i) ) i += 1;
    }
  else 
    { 
      if( arg <= (Min_Int_d - 1.0) ) goto raise_ceil;
      i = (int) arg;
    }
  return convertIntToML(i);

 raise_ceil:
  raise_exn((int)&exn_OVERFLOW);
  return 0;                          // never reached
}

int 
sqrtFloat(int d, int s) 
{ 
  get_d(d) = sqrt(get_d(s)); 
  set_dtag(d);
  return d;
}

int 
sinFloat(int d, int s) 
{
  get_d(d) = sin(get_d(s));
  set_dtag(d);
  return d;
}

int 
cosFloat(int d, int s) 
{
  get_d(d) = cos(get_d(s));
  set_dtag(d);
  return d;
}

int 
atanFloat (int d, int s) 
{
  get_d (d) = atan (get_d (s));
  set_dtag(d);
  return d;
}

int 
asinFloat (int d, int s) 
{
  get_d (d) = asin (get_d (s));
  set_dtag(d);
  return d;
}

int 
acosFloat (int d, int s) 
{
  get_d (d) = acos (get_d (s));
  set_dtag(d);
  return d;
}

int 
atan2Float (int d, int y, int x) 
{
  get_d (d) = atan2 (get_d (y), get_d (x));
  set_dtag(d);
  return d;
}

int expFloat(int d, int s) 
{
  get_d(d) = exp(get_d(s));
  set_dtag(d);
  return d;
}

int 
powFloat (int d, int x, int y) 
{
  get_d (d) = pow (get_d (x), get_d (y));
  set_dtag(d);
  return d;
}

int 
lnFloat(int d, int s) 
{
  get_d(d) = log(get_d(s));
  set_dtag(d);
  return d;
}

int 
sinhFloat(int d, int s) 
{
  get_d(d) = sinh(get_d(s));
  set_dtag(d);
  return d;
}

int 
coshFloat(int d, int s) 
{
  get_d(d) = cosh(get_d(s));
  set_dtag(d);
  return d;
}

int 
tanhFloat(int d, int s) 
{
  get_d(d) = tanh(get_d(s));
  set_dtag(d);
  return d;
}

int 
isnanFloat(int s) 
{
  if (isnan(get_d(s))) 
    {
      return mlTRUE;
    }
  return mlFALSE;
}

int 
posInfFloat(int d) 
{
  get_d(d) = HUGE_VAL;
  set_dtag(d);
  return d;
}

int 
negInfFloat(int d) 
{
  get_d(d) = -HUGE_VAL;
  set_dtag(d);
  return d;
}

// countChar: count the number of times the character `c' 
//     occurs in the string `s'.
static int countChar(int c, char * s) {
  char *p; 
  int count;

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
REG_POLY_FUN_HDR(stringOfFloat, Region rAddr, int arg) 
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
REG_POLY_FUN_HDR(generalStringOfFloat, Region rAddr, String format, int f) 
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
printReal(int f) 
{
  printf("Num: %5.2f\n",get_d(f));
  return;
}

String
REG_POLY_FUN_HDR(sml_real_to_bytes, Region rAddr, int f)
{
  int i;
  double a[1];
  char v[sizeof(double) + 1];
  a[0] = get_d(f);
  for (i = 0; i < sizeof(double) ; i++)
    v[i] = ((char*)a)[i];
  v[sizeof(double)] = 0;
  return REG_POLY_CALL(convertBinStringToML, rAddr, 8, v);
}

int 
sml_bytes_to_real(int d, String s)
{
  double r;
  char* a = &(s->data);
  r = ((double*)a)[0];
  get_d(d) = r;
  set_dtag(d);
  return d;
}
