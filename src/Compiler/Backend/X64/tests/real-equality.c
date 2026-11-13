#include <stdint.h>
#include <stdio.h>
#include <string.h>
extern long equality_boolean(double, double);
extern long equality_branch(double, double);
extern long inequality_boolean(double, double);
extern long inequality_branch(double, double);
int main(void) {
  const uint64_t bits[] = {
    UINT64_C(0), UINT64_C(0x8000000000000000),
    UINT64_C(0x7ff8000000000001), UINT64_C(0xfff8000000000001),
    UINT64_C(0x7ff0000000000001), UINT64_C(0xfff0000000000001),
    UINT64_C(0x7ff0000000000000), UINT64_C(0xfff0000000000000),
    UINT64_C(0x3ff0000000000000), UINT64_C(0xbff0000000000000),
    UINT64_C(1), UINT64_C(0x7fefffffffffffff)
  };
  for (unsigned i=0; i<sizeof bits/sizeof *bits; ++i)
    for (unsigned j=0; j<sizeof bits/sizeof *bits; ++j) {
      double x,y;
      memcpy(&x,&bits[i],8); memcpy(&y,&bits[j],8);
      int expected = !(i>=2 && i<=5) && !(j>=2 && j<=5) &&
                     (i==j || (i<2 && j<2));
      if (equality_boolean(x,y)!=(expected?3:1) ||
          equality_branch(x,y)!=(expected?17:23) ||
          inequality_boolean(x,y)!=(expected?1:3) ||
          inequality_branch(x,y)!=(expected?23:17)) {
        fprintf(stderr,"equality mismatch: %u %u\n",i,j); return 1;
      }
    }
  puts("X64 real equality/inequality: 144 pairs OK");
  return 0;
}
