#ifndef MLKIT_REAL16_H
#define MLKIT_REAL16_H
#include <stdint.h>
/* Core operations take and return ML scalars; conversion helpers use raw bits. */
uintptr_t __plus_real16(uintptr_t, uintptr_t);
uintptr_t __minus_real16(uintptr_t, uintptr_t);
uintptr_t __mul_real16(uintptr_t, uintptr_t);
uintptr_t __div_real16(uintptr_t, uintptr_t);
uintptr_t __sqrt_real16(uintptr_t);
uintptr_t __neg_real16(uintptr_t);
uintptr_t __abs_real16(uintptr_t);
uintptr_t __int_to_real16(intptr_t);
uintptr_t __less_real16(uintptr_t, uintptr_t);
uintptr_t __lesseq_real16(uintptr_t, uintptr_t);
uintptr_t __greater_real16(uintptr_t, uintptr_t);
uintptr_t __greatereq_real16(uintptr_t, uintptr_t);
uintptr_t __equal_real16(uintptr_t, uintptr_t);
uintptr_t __unequal_real16(uintptr_t, uintptr_t);
double mlkit_real16_to_double(uint64_t);
uint64_t mlkit_double_to_real16(double);
uintptr_t mlkit_real16_ldexp(uintptr_t, intptr_t);
uintptr_t mlkit_real16_rem(uintptr_t, uintptr_t);
#endif
