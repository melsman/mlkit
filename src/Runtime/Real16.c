/* _Float16 bit transport uses the platform binary16 ABI. Native arithmetic is
 * selected for scalar FP16 targets and the validated GCC 15+ emulation.
 * Other baseline targets use binary64 plus one exact half-rounding step.
 * Half sums/products are exact in binary64; half quotients and square roots
 * cannot cross a half rounding boundary through binary64 rounding (see tests).
 */
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <fenv.h>
#include "Tagging.h"
#include "Real16.h"

#if !defined(__FLT16_MANT_DIG__) || __FLT16_MANT_DIG__ != 11 || __FLT16_MAX_EXP__ != 16
#error "Real16 requires a C compiler supporting IEEE binary16 _Float16"
#endif
_Static_assert(sizeof(_Float16) == 2, "_Float16 must occupy two bytes");
/* GCC 15+ software arithmetic passes the same dynamic-rounding checks as
 * hardware FP16. Baseline Clang narrowing does not honor directed modes. */
#if !defined(MLKIT_REAL16_FORCE_SOFTWARE) && \
    (defined(__ARM_FEATURE_FP16_SCALAR_ARITHMETIC) || defined(__AVX512FP16__) || \
     (defined(__GNUC__) && !defined(__clang__) && __GNUC__ >= 15))
#define USE_HALF_ARITHMETIC 1
#else
#define USE_HALF_ARITHMETIC 0
#endif
#if defined(__clang__)
#pragma STDC FENV_ACCESS ON
#endif

static _Float16 half(uint16_t h) { _Float16 x; memcpy(&x,&h,2); return x; }
static uint16_t bits(_Float16 x) { uint16_t h; memcpy(&h,&x,2); return h; }
static uint16_t decode(uintptr_t h) { return (uint16_t)convertIntToC(h); }
static uintptr_t encode(uint16_t h) { return convertIntToML(h); }
static int away(int mode, unsigned sign) {
  return (mode == FE_DOWNWARD && sign) || (mode == FE_UPWARD && !sign);
}
static uint16_t overflow(unsigned sign, int mode) {
  feraiseexcept(FE_OVERFLOW | FE_INEXACT);
  return sign | ((mode == FE_TONEAREST || away(mode,sign)) ? 0x7c00 : 0x7bff);
}
/* Compiler emulation of _Float16 narrowing need not honor directed rounding.
 * Decode the binary64 significand instead; the only variable shift is 42..53. */
static uint16_t round_half(double x) {
  uint64_t raw;
  memcpy(&raw,&x,8);
  unsigned sign = (raw >> 48)&0x8000, exp = (raw >> 52)&2047;
  uint64_t m = raw&UINT64_C(0xfffffffffffff);
  if (exp == 2047) return sign | (m ? 0x7e00 | (m >> 42) : 0x7c00);
  if (!(raw&UINT64_C(0x7fffffffffffffff))) return sign;
  int mode = fegetround(), e = (int)exp-1023;
  if (e > 15) return overflow(sign,mode);
  if (e < -25) {
    feraiseexcept(FE_UNDERFLOW | FE_INEXACT);
    return sign | (away(mode,sign) ? 1 : 0);
  }
  m |= UINT64_C(0x10000000000000);
  int shift = 42 + (e < -14 ? -14-e : 0);
  uint64_t q = m >> shift, r = m & ((UINT64_C(1) << shift)-1);
  uint64_t midpoint = UINT64_C(1) << (shift-1);
  if (r) {
    if (away(mode,sign) || (mode == FE_TONEAREST &&
        (r > midpoint || (r == midpoint && (q&1))))) ++q;
    feraiseexcept(FE_INEXACT | (e < -14 ? FE_UNDERFLOW : 0));
  }
  unsigned h = (e < -14 ? 0 : (e+14)*1024) + (unsigned)q;
  return h >= 0x7c00 ? overflow(sign,mode) : sign|h;
}
static uint16_t narrow(double x) {
#if USE_HALF_ARITHMETIC
  return bits((_Float16)x);
#else
  return round_half(x);
#endif
}
#define BINARY(name,op) \
uintptr_t __##name##_real16(uintptr_t a, uintptr_t b) { \
  _Float16 x = half(decode(a)), y = half(decode(b)); \
  if (USE_HALF_ARITHMETIC) return encode(bits((_Float16)(x op y))); \
  return encode(round_half((double)x op (double)y)); }
BINARY(plus,+)
BINARY(minus,-)
BINARY(mul,*)
BINARY(div,/)
/* Hardware sqrt avoids requiring a half-precision libm entry point. Volatile
 * assembly plus a compiler barrier keeps the dynamic rounding environment. */
uintptr_t __sqrt_real16(uintptr_t a) {
#if !defined(MLKIT_REAL16_FORCE_SOFTWARE) && defined(__ARM_FEATURE_FP16_SCALAR_ARITHMETIC)
  _Float16 x = half(decode(a)), r;
  __asm__ volatile("fsqrt %h0, %h1" : "=w"(r) : "w"(x) : "memory");
  return encode(bits(r));
#elif !defined(MLKIT_REAL16_FORCE_SOFTWARE) && defined(__AVX512FP16__)
  _Float16 x = half(decode(a)), r;
  __asm__ volatile("vsqrtsh {%1,%1,%0|%0,%1,%1}" : "=x"(r) : "x"(x) : "memory");
  return encode(bits(r));
#else
  return encode(narrow(sqrt((double)half(decode(a)))));
#endif
}
uintptr_t __neg_real16(uintptr_t a) { return encode(decode(a)^0x8000); }
uintptr_t __abs_real16(uintptr_t a) { return encode(decode(a)&0x7fff); }
uintptr_t __int_to_real16(intptr_t a) {
  /* Integers near the half overflow boundary are exactly representable in
   * binary64; all larger magnitudes overflow half regardless of lost bits. */
  return encode(narrow((double)convertIntToC(a)));
}
#define CMP(name,op) \
uintptr_t __##name##_real16(uintptr_t a, uintptr_t b) { \
  return convertBoolToML(half(decode(a)) op half(decode(b))); }
CMP(less,<)
CMP(lesseq,<=)
CMP(greater,>)
CMP(greatereq,>=)
CMP(equal,==)
CMP(unequal,!=)

double mlkit_real16_to_double(uint64_t h) {
  /* Widen NaNs by copying their payload, without an arithmetic conversion. */
  if ((h&0x7fff) > 0x7c00) {
    uint64_t raw = ((h&0x8000) << 48) | UINT64_C(0x7ff0000000000000) | ((h&1023) << 42);
    double x; memcpy(&x,&raw,8); return x;
  }
  return (double)half((uint16_t)h);
}
uint64_t mlkit_double_to_real16(double x) {
  /* Retain the established explicit NaN payload conversion on both paths. */
  uint64_t raw;
  memcpy(&raw,&x,8);
  uint64_t m = raw&UINT64_C(0xfffffffffffff);
  if (((raw >> 52)&2047) == 2047 && m)
    return ((raw >> 48)&0x8000) | 0x7e00 | (m >> 42);
  return narrow(x);
}
uintptr_t mlkit_real16_ldexp(uintptr_t a, intptr_t exponent) {
  intptr_t e = convertIntToC(exponent);
  if (e > 10000) e = 10000;
  if (e < -10000) e = -10000;
  return encode(narrow(scalbn((double)half(decode(a)),(int)e)));
}
uintptr_t mlkit_real16_rem(uintptr_t a, uintptr_t b) {
  return encode(narrow(fmod((double)half(decode(a)),(double)half(decode(b)))));
}
