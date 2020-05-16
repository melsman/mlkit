#include <stdio.h>

long int incr(long int);

void
work() {
  long int b;
  long int a = 8;
  printf("Calling ML function \"incr\"\n");
  b = incr(a);
  printf("incr(%ld) = %ld\n", a,b);
  printf("Calling ML function \"incr\" again\n");
  b = incr(a);
  printf("incr(%ld) = %ld\n", a,b);
}
