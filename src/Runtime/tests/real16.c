/* A deliberately different oracle: search adjacent representable values.
 * A half has 11 significant bits and a 29-bit exponent span. Binary64 computes
 * half sums/products exactly. For quotients, distance from a half midpoint is
 * either zero or bounded by the 11-bit divisor and 12-bit midpoint denominator;
 * for sqrt, compare the squared midpoint (at most 24 bits). Thus a binary64
 * result cannot cross a half rounding boundary through intermediate rounding.
 */
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <fenv.h>
#include "Tagging.h"
#include "Real16.h"

static double values[0x7c00];
static double value(unsigned h) {
  unsigned a = h&0x7fff;
  double v = a > 0x7c00 ? NAN : a == 0x7c00 ? INFINITY : values[a];
  return h&0x8000 ? -v : v;
}
static unsigned reference(double x, int mode) {
  unsigned sign = signbit(x) ? 0x8000 : 0;
  if (isnan(x)) return 0x7e00;
  x = fabs(x);
  if (isinf(x)) return sign|0x7c00;
  unsigned lo = 0, hi = 0x7c00;
  while (lo+1 < hi) {
    unsigned mid = (lo+hi)/2;
    if (values[mid] <= x) lo = mid; else hi = mid;
  }
  if (x == values[lo]) return sign|lo;
  if (mode == FE_TOWARDZERO || (mode == FE_UPWARD && sign) ||
      (mode == FE_DOWNWARD && !sign)) return sign|lo;
  if (mode != FE_TONEAREST) return sign|hi;
  double midpoint = (values[lo] + (hi == 0x7c00 ? 65536.0 : values[hi]))/2;
  return sign | (x < midpoint || (x == midpoint && !(lo&1)) ? lo : hi);
}
static uintptr_t ml(unsigned h) { return convertIntToML(h); }
static unsigned raw(uintptr_t h) { return convertIntToC(h); }
static int nanbits(unsigned h) { return (h&0x7fff) > 0x7c00; }
static void check(const char *op, unsigned a, unsigned b, unsigned got, unsigned expected) {
  if (got != expected && !(nanbits(got) && nanbits(expected))) {
    fprintf(stderr,"%s(%04x,%04x): got %04x, expected %04x, mode %d\n",
            op,a,b,got,expected,fegetround());
    exit(1);
  }
}
int main(void) {
  for (unsigned h = 0; h < 0x7c00; ++h) {
    unsigned e = h>>10, m = h&1023;
    values[h] = e ? ldexp(1024.0+m,(int)e-25) : ldexp((double)m,-24);
  }
  const int modes[] = {FE_TONEAREST,FE_DOWNWARD,FE_UPWARD,FE_TOWARDZERO};
  for (unsigned mode = 0; mode < 4; ++mode) {
    assert(fesetround(modes[mode]) == 0);
    for (unsigned h = 0; h < 65536; ++h) {
      double x = value(h);
      check("widen",h,0,reference(mlkit_real16_to_double(h),modes[mode]),h);
      check("convert",h,0,mlkit_double_to_real16(x),h);
      check("sqrt",h,0,raw(__sqrt_real16(ml(h))),reference(sqrt(x),modes[mode]));
      check("neg",h,0,raw(__neg_real16(ml(h))),h^0x8000);
      check("abs",h,0,raw(__abs_real16(ml(h))),h&0x7fff);
    }
    for (unsigned h = 0; h < 0x7c00; ++h) {
      double mid = (values[h] + (h == 0x7bff ? 65536.0 : values[h+1]))/2;
      const double probes[] = {nextafter(mid,-INFINITY),mid,nextafter(mid,INFINITY)};
      for (unsigned i = 0; i < 3; ++i)
        for (unsigned neg = 0; neg < 2; ++neg) {
          double x = neg ? -probes[i] : probes[i];
          check("midpoint",h,i,mlkit_double_to_real16(x),reference(x,modes[mode]));
        }
    }
    uint32_t state = 257;
    for (unsigned i = 0; i < 10000; ++i) {
      state = state*1664525+1013904223;
      unsigned a = state>>16;
      state = state*1664525+1013904223;
      unsigned b = state>>16;
      volatile double x = value(a), y = value(b);
      check("add",a,b,raw(__plus_real16(ml(a),ml(b))),reference(x+y,modes[mode]));
      check("sub",a,b,raw(__minus_real16(ml(a),ml(b))),reference(x-y,modes[mode]));
      check("mul",a,b,raw(__mul_real16(ml(a),ml(b))),reference(x*y,modes[mode]));
      check("div",a,b,raw(__div_real16(ml(a),ml(b))),reference(x/y,modes[mode]));
      check("rem",a,b,raw(mlkit_real16_rem(ml(a),ml(b))),reference(fmod(x,y),modes[mode]));
      assert(__less_real16(ml(a),ml(b)) == (uintptr_t)convertBoolToML(x<y));
      assert(__lesseq_real16(ml(a),ml(b)) == (uintptr_t)convertBoolToML(x<=y));
      assert(__greater_real16(ml(a),ml(b)) == (uintptr_t)convertBoolToML(x>y));
      assert(__greatereq_real16(ml(a),ml(b)) == (uintptr_t)convertBoolToML(x>=y));
      assert(__equal_real16(ml(a),ml(b)) == (uintptr_t)convertBoolToML(x==y));
      assert(__unequal_real16(ml(a),ml(b)) == (uintptr_t)convertBoolToML(x!=y));
    }
    for (int n = -65536; n <= 65536; ++n)
      check("integer",n,0,raw(__int_to_real16(convertIntToML(n))),reference(n,modes[mode]));
  }
  assert(fesetround(FE_TONEAREST) == 0);
  puts("binary16 runtime checks passed");
}
