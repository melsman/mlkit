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

static double value(unsigned h) {
  unsigned a = h&0x7fff;
  double v = a > 0x7c00 ? NAN : a == 0x7c00 ? INFINITY :
    (a >> 10 ? ldexp(1024.0+(a&1023),(int)(a>>10)-25) : ldexp((double)a,-24));
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
    if (value(mid) <= x) lo = mid; else hi = mid;
  }
  if (x == value(lo)) return sign|lo;
  if (mode == FE_TOWARDZERO || (mode == FE_UPWARD && sign) ||
      (mode == FE_DOWNWARD && !sign)) return sign|lo;
  if (mode != FE_TONEAREST) return sign|hi;
  double midpoint = (value(lo) + (hi == 0x7c00 ? 65536.0 : value(hi)))/2;
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
  /* Boundaries and their neighbors: zeros, subnormal/normal transition,
   * powers of two, rounding ties, overflow, infinities and NaNs. */
  const unsigned cases[] = {0,1,2,0x3fe,0x3ff,0x400,0x401,0x3555,
    0x3bff,0x3c00,0x3c01,0x3c02,0x4000,0x63ff,0x6400,0x7bfe,0x7bff,
    0x7c00,0x7c01,0x7e55};
  const int64_t integers[] = {-65536,-65520,-65504,-2051,-2049,-1,0,1,2049,2051,65504,65520,65536,
    -(INT64_C(1)<<62),(INT64_C(1)<<62)-1
#ifndef TAG_VALUES
    ,INT64_MIN,INT64_MAX
#endif
  };
  const int modes[] = {FE_TONEAREST,FE_DOWNWARD,FE_UPWARD,FE_TOWARDZERO};
  for (unsigned mode = 0; mode < 4; ++mode) {
    assert(fesetround(modes[mode]) == 0);
    for (unsigned i = 0; i < sizeof cases / sizeof cases[0]; ++i)
      for (unsigned sign = 0; sign < 2; ++sign) {
        unsigned h = cases[i] | (sign ? 0x8000 : 0);
        double x = value(h);
        check("widen",h,0,reference(mlkit_real16_to_double(h),modes[mode]),h);
        check("convert",h,0,mlkit_double_to_real16(x),h);
        check("sqrt",h,0,raw(__sqrt_real16(ml(h))),reference(sqrt(x),modes[mode]));
        check("neg",h,0,raw(__neg_real16(ml(h))),h^0x8000);
        check("abs",h,0,raw(__abs_real16(ml(h))),h&0x7fff);
      }
    for (unsigned i = 0; i < sizeof cases / sizeof cases[0]; ++i) {
      unsigned h = cases[i];
      if (h >= 0x7c00) continue;
      double mid = (value(h) + (h == 0x7bff ? 65536.0 : value(h+1)))/2;
      const double probes[] = {nextafter(mid,-INFINITY),mid,nextafter(mid,INFINITY)};
      for (unsigned i = 0; i < 3; ++i)
        for (unsigned neg = 0; neg < 2; ++neg) {
          double x = neg ? -probes[i] : probes[i];
          check("midpoint",h,i,mlkit_double_to_real16(x),reference(x,modes[mode]));
        }
    }
    for (unsigned i = 0; i < sizeof cases / sizeof cases[0]; ++i)
      for (unsigned j = 0; j < sizeof cases / sizeof cases[0]; ++j) {
        unsigned a = cases[i], b = cases[j] | (i&1 ? 0x8000 : 0);
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
    check("negative zero sum",0x8000,0x8000,
          raw(__plus_real16(ml(0x8000),ml(0x8000))),0x8000);
    for (unsigned i = 0; i < sizeof integers / sizeof integers[0]; ++i) {
      int64_t n = integers[i];
      check("integer",n,0,raw(__int_to_real16(convertIntToML(n))),reference(n,modes[mode]));
    }
  }
  assert(fesetround(FE_TONEAREST) == 0);
  puts("binary16 runtime checks passed");
}
