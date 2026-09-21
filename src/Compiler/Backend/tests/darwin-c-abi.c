/* Independent Apple-toolchain oracle for the placement rules in AbiArm64. */
#include <assert.h>
#include <stdio.h>
extern long mlkit_abi_packed(signed char,signed char,signed char,signed char,
                            signed char,signed char,signed char,signed char,
                            signed char,signed char);
extern long mlkit_abi_varargs(const char *, ...);
extern long mlkit_abi_callback(long (*)(long,double));
static long callback(long n, double d) { return n == 41 && d == 1.5; }
int main(void) {
  assert(mlkit_abi_packed(-1,-2,-3,-4,-5,-6,-7,-8,-9,-10));
  assert(mlkit_abi_varargs("test", (signed char)-7, (float)1.5, 99L));
  assert(mlkit_abi_callback(callback));
  puts("Darwin scalar ABI probe passed");
}
