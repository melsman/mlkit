#include <stdio.h>
#include <stdlib.h>
#include <time.h>
extern unsigned long b_ret(unsigned long), b_br(unsigned long), bl_ret(unsigned long), bl_br(unsigned long);
int main(int argc, char **argv) {
  unsigned long (*f[])(unsigned long) = {b_ret,b_br,bl_ret,bl_br};
  struct timespec a,b;
  if (argc != 2) return 1;
  int mode=atoi(argv[1]); if (mode<0 || mode>3) return 1;
  clock_gettime(CLOCK_MONOTONIC,&a);
  unsigned long result=f[mode](10000000);
  clock_gettime(CLOCK_MONOTONIC,&b);
  if(result!=10000000) return 2;
  printf("%.9f\n",(b.tv_sec-a.tv_sec)+(b.tv_nsec-a.tv_nsec)*1e-9);
  return 0;
}
