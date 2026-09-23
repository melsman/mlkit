#include <assert.h>
long arm64_nonzero(long b) { assert(b==1); return -7; }
long arm64_raw10(long a,long b,long c,long d,long e,long f,long g,long h,long i,long j) {
  assert(a<b && b<c && c<d && d<e && e<f && f<g && g<h && h<i && i<j);
  return j;
}
