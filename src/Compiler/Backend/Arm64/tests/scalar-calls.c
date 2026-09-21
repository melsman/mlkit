#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
long arm64_mixed_check(long a,long b,long c,long d,long e,long f,long g,long h,
                      signed char i,unsigned char j,double x0,double x1,double x2,
                      double x3,double x4,double x5,double x6,double x7,double x8,double x9,short k) {
  assert(a==1 && b==2 && c==3 && d==4 && e==5 && f==6 && g==7 && h==8);
  assert(i==-3 && j==250 && k==-1234);
  assert(x0==1 && x1==2 && x2==3 && x3==4 && x4==5 && x5==6 && x6==7 && x7==8 && x8==9 && x9==10);
  return 17;
}
long arm64_variadic_check(int n,...) {
  assert(n==4);
  va_list args; va_start(args,n);
  assert(va_arg(args,int)==-3);
  assert(va_arg(args,double)==1.5);
  assert(va_arg(args,double)==2.5);
  assert(va_arg(args,long)==99);
  va_end(args); return 19;
}
double arm64_float_result_check(double x) { return x+0.25; }
long arm64_narrow_check(signed char a,unsigned short b,int c) {
  assert(a==-5 && b==60000 && c==17); return 23;
}
extern double arm64_float_result_probe(void);
extern long arm64_narrow_probe(void);
extern long arm64_mixed_probe(void),arm64_variadic_probe(void);
int main(void) {
  assert(arm64_mixed_probe()==17 && arm64_variadic_probe()==19);
  assert(arm64_float_result_probe()==1.25 && arm64_narrow_probe()==23);
  puts("ARM64 generated mixed, packed-stack, and variadic C calls passed");
}
