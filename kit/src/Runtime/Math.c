/*----------------------------------------------------------------*
 *                        Math                                    *
 *----------------------------------------------------------------*/
#include <errno.h>
#include <math.h>
#include "Math.h"
#include "Tagging.h"
#include "Exception.h"

unsigned int max(unsigned int a, unsigned int b) {
  return (a<b)?b:a;
}


/*------------------------------------------------------------------------*
 *                         ML Integer Functions                           *
 *                                                                        *
 * div_int_: Calculate x div y, where x and y are represented as 2i+1.    *
 * mod_int_: Calculate x mod y, where x and y are represented as 2i+1.    *
 *------------------------------------------------------------------------*/


#if TAG_INTEGERS

int div_int_(int x, int y, int exn)
{
  if (y == 1) { 
    raise_exn(exn);
    return;
  }
  else {
    if (x == 1) return 1;
    if (x < 1 && y > 1)
      return (2*((x+1)/(y-1))-1);
    else if (x > 1 && y < 1)
      return (2*((x-3)/(y-1))-1);
    else return (2*((x-1)/(y-1))+1);
  }
}

int mod_int_(int xML, int yML, int exn)
{
  if (yML == 1) {
    raise_exn(exn);
    return;
  }
  else
    if ((xML-1)%(yML-1) == 0 || (xML>1 && yML>1) || (xML<1 && yML<1))
      return ((xML-1)%(yML-1))+1;
    else
      return ((xML-1)%(yML-1))+yML;
}

#else /* Don't tag integers */

int div_int_(int x, int y, int exn)
{
  if (y == 0) {
    raise_exn(exn);
    return;
  }
  else {
    if (x < 0 && y > 0)
      return ((x + 1) / y) - 1;
    else if (x > 0 && y < 0)
      return ((x - 1) / y) - 1;
    else return x / y;
  }
}

int mod_int_(int x, int y, int exn)
{
  if (y == 0) {
    raise_exn(exn);
    return;
  }
  else {
    if ( (x > 0 && y > 0) || (x < 0 && y < 0) || (x % y == 0) ) return x % y;
    return (x % y) + y;
  }
}

unsigned int div_word_(unsigned int x, unsigned int y, int exn)
{
  if (y == 0) {
    raise_exn(exn);
    return;
  }
  return x / y;
}

unsigned int mod_word_(unsigned int x, unsigned int y, int exn)
{
  if (y == 0) {
    raise_exn(exn);
    return;
  }
  return x % y;
}

int quotInt(int x, int y)
{
  return x / y;
}

int remInt(int x, int y)
{
  return x % y;
}


#endif /*TAG_INTEGERS*/

int realInt(int d, int x)
{
  get_d(d) = (double) (convertIntToC(x));
  set_dtag(d);
  return d;
}

/*----------------------------------------------------------------------*
 *                                                                      *
 *                      Floating point operations                       *
 *                                                                      *
 *----------------------------------------------------------------------*/

int divFloat(int d, int x, int y)
{
  get_d(d) = get_d(x) / get_d(y);
  set_dtag(d);
  return d;
}


int floorFloat(int f)
{ double r;
  int i;

  r = get_d(f);
  if( r >= 0.0 )
    { if( r >= (((double)Max_Int)+1.0) ) goto raise_floor;
      i = (int) r;
    }
  else
    { 
      if( r < ((double) Min_Int) ) goto raise_floor;
      i = (int) r;
      if( r < ((double) i) ) i -= 1;
    }
  return convertIntToML(i);

raise_floor:
    raise_exn((int)&exn_OVERFLOW);
    return;
}

int truncFloat(int f)
{ 
  double r;

  r = get_d(f);
  if ((r >= (((double)Max_Int) + 1.0)) || (r <= (((double)Min_Int)-1.0))) { 
    raise_exn((int)&exn_OVERFLOW);
    return;
  }
  return convertIntToML((int)r);
}


int ceilFloat(int f)
{ 
  double arg;
  int i;

  arg = get_d(f);

  if( arg >= 0.0 ) { 
    if( arg > Max_Int_d ) goto raise_ceil;
    i = (int) arg;
    if( arg > ((double) i) ) i += 1;
  }
  else { 
    if( arg <= (Min_Int_d - 1.0) ) goto raise_ceil;
    i = (int) arg;
  }
  return convertIntToML(i);

raise_ceil:
    raise_exn((int)&exn_OVERFLOW);
    return;
}



int sqrtFloat(int d, int s)
{ 
  get_d(d) = sqrt(get_d(s)); 
  set_dtag(d);
  return d;
}

int sinFloat(int d, int s)
{
  get_d(d) = sin(get_d(s));
  set_dtag(d);
  return d;
}

int cosFloat(int d, int s)
{
  get_d(d) = cos(get_d(s));
  set_dtag(d);
  return d;
}

int atanFloat (int d, int s)
{
  get_d (d) = atan (get_d (s));
  set_dtag(d);
  return d;
}

int asinFloat (int d, int s)
{
  get_d (d) = asin (get_d (s));
  set_dtag(d);
  return d;
}

int acosFloat (int d, int s)
{
  get_d (d) = acos (get_d (s));
  set_dtag(d);
  return d;
}

int atan2Float (int d, int y, int x)
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

int powFloat (int d, int x, int y)
{
  get_d (d) = pow (get_d (x), get_d (y));
  set_dtag(d);
  return d;
}

int lnFloat(int d, int s)
{
  get_d(d) = log(get_d(s));
  set_dtag(d);
  return d;
}

int sinhFloat(int d, int s)
{
  get_d(d) = sinh(get_d(s));
  set_dtag(d);
  return d;
}

int coshFloat(int d, int s)
{
  get_d(d) = cosh(get_d(s));
  set_dtag(d);
  return d;
}

int tanhFloat(int d, int s)
{
  get_d(d) = tanh(get_d(s));
  set_dtag(d);
  return d;
}

int isnanFloat(int s) {
  int b;
  b = mlFALSE;
  if (isnan(get_d(s))) b = mlTRUE;

  return b;
}

int posInfFloat(int d) {
  get_d(d) = HUGE_VAL;
  set_dtag(d);
  return d;
}

int negInfFloat(int d) {
  get_d(d) = -HUGE_VAL;
  set_dtag(d);
  return d;
}

/* Count the number of times the character `c' */
/* occurs in the string `s'. */

static int countChar(int c, char * s)
{
  char *p; int count;

  count = 0;
  for( p=s; *p != '\0'; p++ ) {
    if( *p == c ) count++;
  }
  return count;
}

/* Here we remove all '+', and replace '-' and 'e' */
/* with '~' and 'E', respectively. */

static void mkSMLMinus(char * s)
{
  char *p, *q;

  for( p=s, q=s; *p != '\0'; p++ ) {
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


StringDesc* stringOfFloat(int rAddr, int arg) {   /* ML */
  StringDesc* res;
  char result_buffer[64];
  unsigned int sz;
  unsigned int i;

  sprintf(result_buffer, "%.12g", get_d(arg));
  mkSMLMinus(result_buffer);
  if( countChar('.', result_buffer) == 0 &&
      countChar('E', result_buffer) == 0 )
    strcat(result_buffer, ".0");

  sz = strlen(result_buffer);
  res = allocString(rAddr, sz);

  for (i=0; i < sz; i++)
    updateString(res, i, (int)(result_buffer[i]));
    
  return res;
}

#ifdef PROFILING
StringDesc* stringOfFloatProf(int rAddr, int arg, int pPoint) {   /* ML */
  StringDesc* res;
  char result_buffer[64];
  unsigned int sz;
  unsigned int i;

  sprintf(result_buffer, "%.12g", get_d(arg));
  mkSMLMinus(result_buffer);
  if( countChar('.', result_buffer) == 0 &&
      countChar('E', result_buffer) == 0 )
    strcat(result_buffer, ".0");

  sz = strlen(result_buffer);
  res = allocStringProfiling(rAddr, sz, pPoint);

  for (i=0; i < sz; i++)
    updateString(res, i, (int)(result_buffer[i]));
    
  return res;
}
#endif /* PROFILING */

StringDesc* generalStringOfFloat(int rAddr, StringDesc *str, int f)
{
  StringDesc* res;
  char result_buffer[512];
  unsigned int sz;
  unsigned int i;

  /* Unfortunately there seems to be no way to ensure that this does not
   * crash by overflowing the result_buffer (e.g. when specifying a huge 
   * number of decimal digits in the fixed-point format): 
   */

  convertString(str);   /* convert str to a C string and put in cString. */

  sprintf(result_buffer, cString, get_d(f));

  mkSMLMinus(result_buffer);

  sz = strlen(result_buffer);

  res = allocString(rAddr, sz);

  for (i=0; i < sz; i++)
    updateString(res, i, (int)(result_buffer[i]));
    
  return res;
}

#ifdef PROFILING
StringDesc* generalStringOfFloatProf(int rAddr, StringDesc *str, int f, int pPoint)
{
  StringDesc* res;
  char result_buffer[512];
  unsigned int sz;
  unsigned int i;

  /* Unfortunately there seems to be no way to ensure that this does not
   * crash by overflowing the result_buffer (e.g. when specifying a huge 
   * number of decimal digits in the fixed-point format): 
   */

  convertString(str);   /* convert str to a C string and put in cString. */

  sprintf(result_buffer, cString, get_d(f));

  mkSMLMinus(result_buffer);

  sz = strlen(result_buffer);

  res = allocStringProfiling(rAddr, sz, pPoint);

  for (i=0; i < sz; i++)
    updateString(res, i, (int)(result_buffer[i]));
    
  return res;
}

#endif /*PROFILING*/

/* DEBUG */
void printReal(double *n) {
  printf("Num: %5.2f\n",*n);
  return;
}
