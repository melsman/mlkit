/* Portable IEEE binary16 operations. No optional FP16 ISA is required.
 * Every finite half is an integer multiple of 2^-24. Addition, multiplication
 * and division therefore fit exact numerators/denominators in 128 bits.
 * Quantize once, using an exact remainder and the current IEEE rounding mode.
 * sqrt compares squared midpoints, so it never rounds through another format.
 */
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <fenv.h>
#include "Tagging.h"
#include "Real16.h"

typedef __uint128_t U128;
static unsigned width(U128 n) {
  uint64_t hi = n >> 64, lo = (uint64_t)n;
  if (hi) return 128 - (unsigned)__builtin_clzll(hi);
  return lo ? 64 - (unsigned)__builtin_clzll(lo) : 0;
}
static int away(int mode, unsigned sign) {
  return (mode == FE_DOWNWARD && sign) || (mode == FE_UPWARD && !sign);
}
static uint16_t overflow(unsigned sign, int mode) {
  feraiseexcept(FE_OVERFLOW | FE_INEXACT);
  return sign | ((mode == FE_TONEAREST || away(mode,sign)) ? 0x7c00 : 0x7bff);
}
/* n/d * 2^exponent, with a separate sign (including signed zero).
 * Bounds at callers guarantee each shifted intermediate fits U128. */
static uint16_t quantize(U128 n, U128 d, int exponent, unsigned sign) {
  if (!n) return sign;
  int mode = fegetround();
  int k = (int)width(n) - (int)width(d);
  int e = k + exponent;
  if ((k >= 0 ? n < (d << k) : (n << -k) < d)) --e;
  if (e > 15) return overflow(sign,mode);
  if (e < -25) {
    feraiseexcept(FE_UNDERFLOW | FE_INEXACT);
    return sign | (away(mode,sign) ? 1 : 0);
  }
  int unit = (e < -14 ? -14 : e) - 10;
  int shift = exponent - unit;
  if (shift >= 0) n <<= shift; else d <<= -shift;
  U128 q = n/d, r = n%d;
  if (r) {
    if (away(mode,sign) || (mode == FE_TONEAREST &&
        (r > d-r || (r == d-r && (q & 1))))) ++q;
    feraiseexcept(FE_INEXACT | (e < -14 ? FE_UNDERFLOW : 0));
  }
  unsigned bits = (e < -14 ? 0 : (e+14)*1024) + (unsigned)q;
  if (bits >= 0x7c00) return overflow(sign,mode);
  return sign | bits;
}
static uint64_t units(uint16_t h) {
  unsigned e = (h >> 10) & 31, m = h & 1023;
  return e ? (uint64_t)(1024+m) << (e-1) : m;
}
static int nan16(uint16_t h) { return (h & 0x7fff) > 0x7c00; }
static int inf16(uint16_t h) { return (h & 0x7fff) == 0x7c00; }
static uint16_t quiet(uint16_t h) {
  if (!(h & 512)) feraiseexcept(FE_INVALID);
  return h | 512;
}
static uint16_t invalid(void) { feraiseexcept(FE_INVALID); return 0x7e00; }
static uint16_t add(uint16_t a, uint16_t b) {
  if (nan16(a)) return quiet(a);
  if (nan16(b)) return quiet(b);
  if (inf16(a)) return inf16(b) && ((a^b)&0x8000) ? invalid() : a;
  if (inf16(b)) return b;
  int64_t x = (int64_t)units(a), y = (int64_t)units(b);
  if (a & 0x8000) x = -x;
  if (b & 0x8000) y = -y;
  int64_t z = x+y;
  unsigned sign = z < 0 ? 0x8000 : 0;
  if (!z) sign = ((a & b & 0x8000) ||
                  (((a^b)&0x8000) && fegetround() == FE_DOWNWARD)) ? 0x8000 : 0;
  return quantize(z < 0 ? -z : z,1,-24,sign);
}
static uint16_t multiply(uint16_t a, uint16_t b) {
  unsigned sign = (a^b)&0x8000;
  if (nan16(a)) return quiet(a);
  if (nan16(b)) return quiet(b);
  if (inf16(a) || inf16(b))
    return !(a&0x7fff) || !(b&0x7fff) ? invalid() : sign|0x7c00;
  return quantize((U128)units(a)*units(b),1,-48,sign);
}
static uint16_t divide(uint16_t a, uint16_t b) {
  unsigned sign = (a^b)&0x8000;
  if (nan16(a)) return quiet(a);
  if (nan16(b)) return quiet(b);
  if (inf16(a)) return inf16(b) ? invalid() : sign|0x7c00;
  if (inf16(b)) return sign;
  if (!(b&0x7fff)) {
    if (!(a&0x7fff)) return invalid();
    feraiseexcept(FE_DIVBYZERO);
    return sign|0x7c00;
  }
  return quantize(units(a),units(b),0,sign);
}
static uint16_t root(uint16_t a) {
  if (nan16(a)) return quiet(a);
  if (!(a&0x7fff)) return a;
  if (a&0x8000) return invalid();
  if (inf16(a)) return a;
  U128 n = units(a), d = 1;
  int e = (int)width(n)-1-24;
  e = e < 0 ? -((-e+1)/2) : e/2;
  int unit = e-10, shift = -24-2*unit;
  if (shift >= 0) n <<= shift; else d <<= -shift;
  unsigned lo = 1024, hi = 2048;
  while (lo+1 < hi) {
    unsigned mid = (lo+hi)/2;
    if ((U128)mid*mid*d <= n) lo = mid; else hi = mid;
  }
  if ((U128)lo*lo*d != n) {
    int mode = fegetround();
    U128 midpoint = (U128)(2*lo+1)*(2*lo+1)*d;
    if (mode == FE_UPWARD || (mode == FE_TONEAREST &&
        (4*n > midpoint || (4*n == midpoint && (lo&1))))) ++lo;
    feraiseexcept(FE_INEXACT);
  }
  return (e+14)*1024+lo;
}
/* These helpers use ML's scalar representation, not C's _Float16 ABI. */
static uint16_t decode(uintptr_t h) { return (uint16_t)convertIntToC(h); }
static uintptr_t encode(uint16_t h) { return convertIntToML(h); }
uintptr_t __plus_real16(uintptr_t a, uintptr_t b) { return encode(add(decode(a),decode(b))); }
uintptr_t __minus_real16(uintptr_t a, uintptr_t b) { return encode(add(decode(a),decode(b)^0x8000)); }
uintptr_t __mul_real16(uintptr_t a, uintptr_t b) { return encode(multiply(decode(a),decode(b))); }
uintptr_t __div_real16(uintptr_t a, uintptr_t b) { return encode(divide(decode(a),decode(b))); }
uintptr_t __sqrt_real16(uintptr_t a) { return encode(root(decode(a))); }
uintptr_t __neg_real16(uintptr_t a) { return encode(decode(a)^0x8000); }
uintptr_t __abs_real16(uintptr_t a) { return encode(decode(a)&0x7fff); }
uintptr_t __int_to_real16(intptr_t a) {
  int64_t x = convertIntToC(a);
  uint64_t n = x < 0 ? (uint64_t)(-(x+1))+1 : (uint64_t)x;
  return encode(quantize(n,1,0,x < 0 ? 0x8000 : 0));
}
static int compare(uint16_t a, uint16_t b) {
  if (nan16(a) || nan16(b)) return 2;
  if (a == b || !((a|b)&0x7fff)) return 0;
  if ((a^b)&0x8000) return (a&0x8000) ? -1 : 1;
  return ((a < b) != !!(a&0x8000)) ? -1 : 1;
}
#define CMP(name,condition) \
uintptr_t __##name##_real16(uintptr_t a, uintptr_t b) { \
  int c = compare(decode(a),decode(b)); return convertBoolToML(condition); }
CMP(less,c == -1)
CMP(lesseq,c == -1 || c == 0)
CMP(greater,c == 1)
CMP(greatereq,c == 1 || c == 0)
CMP(equal,c == 0)
CMP(unequal,c != 0)

/* Raw-bit conversion helpers use automatic C conversion at the ML boundary. */
double mlkit_real16_to_double(uint64_t bits) {
  uint16_t h = bits;
  uint64_t raw;
  if ((h&0x7c00) == 0x7c00) {
    raw = ((uint64_t)(h&0x8000) << 48) | UINT64_C(0x7ff0000000000000) |
          ((uint64_t)(h&1023) << 42);
    double result;
    memcpy(&result,&raw,sizeof result);
    return result;
  }
  double x = ldexp((double)units(h),-24); /* exact: at most 40 bits */
  return h&0x8000 ? -x : x;
}
uint64_t mlkit_double_to_real16(double x) {
  uint64_t raw;
  memcpy(&raw,&x,sizeof raw);
  unsigned sign = (raw >> 48)&0x8000, e = (raw >> 52)&2047;
  uint64_t m = raw&UINT64_C(0xfffffffffffff);
  if (e == 2047) return sign | (m ? 0x7e00 | (m >> 42) : 0x7c00);
  return quantize(e ? m | UINT64_C(0x10000000000000) : m,1,
                  e ? (int)e-1023-52 : -1074,sign);
}
uintptr_t mlkit_real16_ldexp(uintptr_t a, intptr_t exponent) {
  uint16_t h = decode(a);
  if ((h&0x7c00) == 0x7c00) return encode(nan16(h) ? quiet(h) : h);
  intptr_t e = convertIntToC(exponent);
  if (e > 10000) e = 10000;
  if (e < -10000) e = -10000;
  return encode(quantize(units(h),1,(int)e-24,h&0x8000));
}
uintptr_t mlkit_real16_rem(uintptr_t a, uintptr_t b) {
  uint16_t x = decode(a), y = decode(b);
  if (nan16(x)) return encode(quiet(x));
  if (nan16(y)) return encode(quiet(y));
  if (inf16(x) || !(y&0x7fff)) return encode(invalid());
  if (inf16(y)) return a;
  return encode(quantize(units(x)%units(y),1,-24,x&0x8000));
}
