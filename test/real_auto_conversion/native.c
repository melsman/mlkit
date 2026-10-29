#include <assert.h>
#include <stdint.h>
#include <string.h>
#ifdef REAL_AUTO_GC
#include <stddef.h>
#include <sys/types.h>
extern long disable_gc;
extern size_t time_to_gc;
extern ssize_t num_gc;
static ssize_t first_gc;
static int gc_started;
#endif

static const uint64_t patterns[] = {
  UINT64_C(0x8000000000000000), /* negative zero */
  UINT64_C(0x7ff0000000000000), /* positive infinity */
  UINT64_C(0xfff0000000000000), /* negative infinity */
  UINT64_C(0x7ff8000000001234), /* quiet NaN with payload */
  UINT64_C(0x0000000000000001), /* smallest positive subnormal */
  UINT64_C(0x3ff4000000000000)  /* 1.25 */
};
double real_auto_special(long kind) {
  double x;
  assert(kind >= 0 && kind < 6);
  memcpy(&x, &patterns[kind], sizeof x);
  return x;
}
long real_auto_bits(double x, long kind) {
  uint64_t bits;
  memcpy(&bits, &x, sizeof bits);
  assert(kind >= 0 && kind < 6);
  return bits == patterns[kind];
}
double real_auto_identity(double x) { return x; }
double real_auto_sum(double x, double y) { return x + y; }
double real_auto_word(double x, uint64_t w) {
  assert(w == UINT64_C(0x7fffffffffffffff));
  return x;
}
uint64_t real_auto_word_result(double x) {
  assert(x == 2.5);
  return UINT64_C(0x7fffffffffffffff);
}
long real_auto_word_check(uint64_t w) { return w == UINT64_C(0x7fffffffffffffff); }
/* Explicit 31/63-bit types remain tagged even in a no-GC build. */
long real_auto_int31(double marker, long index, long value) {
  static const long expected[] = {-1073741824L, -1L, 0L, 1073741823L};
  assert(marker == 2.5 && index >= 0 && index < 4);
  assert(value == expected[index]);
  return value;
}
long real_auto_int63(double marker, long index, long value) {
  static const long expected[] = {-4611686018427387904L, -1L, 0L, 4611686018427387903L};
  assert(marker == 2.5 && index >= 0 && index < 4);
  assert(value == expected[index]);
  return value;
}
unsigned long real_auto_word31(double marker, long index, unsigned long value) {
  static const unsigned long expected[] = {0UL, 1UL, 1073741824UL, 2147483647UL};
  assert(marker == 2.5 && index >= 0 && index < 4);
  assert(value == expected[index]);
  return value;
}
unsigned long real_auto_word63(double marker, long index, unsigned long value) {
  static const unsigned long expected[] = {0UL, 1UL, 4611686018427387904UL, 9223372036854775807UL};
  assert(marker == 2.5 && index >= 0 && index < 4);
  assert(value == expected[index]);
  return value;
}
long real_auto_precise_stack(double marker, long a, unsigned long b, long c, unsigned long d,
                             long e, unsigned long f, long g, unsigned long h,
                             long i, unsigned long j) {
  assert(marker == 2.5 && a == -1 && b == 2147483647UL);
  assert(c == -4611686018427387904L && d == 9223372036854775807UL);
  assert(e == a && f == b && g == c && h == d && i == a && j == b);
  return -1073741824L;
}

double real_auto_nine(double a, double b, double c, double d, double e,
                      double f, double g, double h, double i) {
  return a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i;
}
double real_auto_mixed(long i0, double r0, long i1, double r1,
                       long i2, double r2, long i3, double r3,
                       long i4, double r4, long i5, double r5,
                       long i6, double r6, long i7, double r7,
                       long i8, double r8, long i9, double r9) {
  assert(((uintptr_t)__builtin_frame_address(0) & 15) == 0);
  assert(i0 == -10 && i1 == 11 && i2 == 12 && i3 == 13 && i4 == 14);
  assert(i5 == 15 && i6 == 16 && i7 == 17 && i8 == 18 && i9 == 19);
  assert(r0 == 0.25 && r1 == 1.25 && r2 == 2.25 && r3 == 3.25 && r4 == 4.25);
  assert(r5 == 5.25 && r6 == 6.25 && r7 == 7.25 && r8 == 8.25 && r9 == 9.25);
  return r9 + i9;
}
double real_auto_other(double x, long b, const char *s) {
  assert(b == 1 && strcmp(s, "abc") == 0);
  return x + b + strlen(s);
}
long real_auto_boxed(double x) { assert(x == 2.5); return 42; }
static int void_seen;
void real_auto_void(double x) { assert(x == 2.5); void_seen = 1; }
long real_auto_void_seen(void) { return void_seen; }
static int order;
double real_auto_left(void) { assert(order == 0); order = 1; return 1.25; }
long real_auto_right(void) { assert(order == 1); order = 2; return 2; }
double real_auto_ordered(double x, long n) {
  assert(order == 2 && x == 1.25 && n == 2);
  order = 3;
  return x + n;
}
void real_auto_never(double x, long n) { (void)x; (void)n; assert(0); }
long real_auto_clobber(long n) {
  /* Exercise every caller-saved FP register, not just those used by C here. */
#if defined(__aarch64__)
  __asm__ volatile(
    "movi v0.16b, #0\n\tmovi v1.16b, #0\n\tmovi v2.16b, #0\n\tmovi v3.16b, #0\n\t"
    "movi v4.16b, #0\n\tmovi v5.16b, #0\n\tmovi v6.16b, #0\n\tmovi v7.16b, #0\n\t"
    "movi v16.16b, #0\n\tmovi v17.16b, #0\n\tmovi v18.16b, #0\n\tmovi v19.16b, #0\n\t"
    "movi v20.16b, #0\n\tmovi v21.16b, #0\n\tmovi v22.16b, #0\n\tmovi v23.16b, #0\n\t"
    "movi v24.16b, #0\n\tmovi v25.16b, #0\n\tmovi v26.16b, #0\n\tmovi v27.16b, #0\n\t"
    "movi v28.16b, #0\n\tmovi v29.16b, #0\n\tmovi v30.16b, #0\n\tmovi v31.16b, #0"
    : : : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7",
    "v16", "v17", "v18", "v19", "v20", "v21", "v22", "v23",
    "v24", "v25", "v26", "v27", "v28", "v29", "v30", "v31");
#elif defined(__x86_64__)
  __asm__ volatile(
    "pxor %%xmm0, %%xmm0\n\tpxor %%xmm1, %%xmm1\n\tpxor %%xmm2, %%xmm2\n\tpxor %%xmm3, %%xmm3\n\t"
    "pxor %%xmm4, %%xmm4\n\tpxor %%xmm5, %%xmm5\n\tpxor %%xmm6, %%xmm6\n\tpxor %%xmm7, %%xmm7\n\t"
    "pxor %%xmm8, %%xmm8\n\tpxor %%xmm9, %%xmm9\n\tpxor %%xmm10, %%xmm10\n\tpxor %%xmm11, %%xmm11\n\t"
    "pxor %%xmm12, %%xmm12\n\tpxor %%xmm13, %%xmm13\n\tpxor %%xmm14, %%xmm14\n\tpxor %%xmm15, %%xmm15"
    : : : "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7",
    "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15");
#else
#error Unsupported real auto-conversion test target
#endif
  return n + 7;
}
double real_auto_gc_result(long n) {
#ifdef REAL_AUTO_GC
  assert(disable_gc == 1);
  if (!gc_started) { first_gc = num_gc; gc_started = 1; }
  time_to_gc = 1;
#endif
  return n + 0.25;
}
long real_auto_gc_verify(void) {
#ifdef REAL_AUTO_GC
  return gc_started && num_gc > first_gc;
#else
  return 1;
#endif
}

/* Float arguments share the FP register bank with doubles. The last arguments
 * spill to the C stack on both supported native targets. */
float real_auto_single_mixed(long marker, double wide,
                            float a, float b, float c, float d, float e,
                            float f, float g, float h, float i, float last) {
  assert(marker == 123 && wide == 2.5);
  assert(a == 1 && b == 2 && c == 3 && d == 4 && e == 5);
  assert(f == 6 && g == 7 && h == 8 && i == 9);
  return last;
}
