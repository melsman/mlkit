#include <assert.h>
long alignment_seed(void) { return 11; }
long alignment_check0(void) {
  return 1;
}
long alignment_check6(long a0, long a1, long a2, long a3, long a4, long a5) {
  assert(a0 == 11 || a0 == 23);
  assert(a1 == a0 + 1 * (a0 == 11 ? 11 : 22));
  assert(a2 == a0 + 2 * (a0 == 11 ? 11 : 22));
  assert(a3 == a0 + 3 * (a0 == 11 ? 11 : 22));
  assert(a4 == a0 + 4 * (a0 == 11 ? 11 : 22));
  assert(a5 == a0 + 5 * (a0 == 11 ? 11 : 22));
  return a5;
}
long alignment_check7(long a0, long a1, long a2, long a3, long a4, long a5, long a6) {
  assert(a0 == 11 || a0 == 23);
  assert(a1 == a0 + 1 * (a0 == 11 ? 11 : 22));
  assert(a2 == a0 + 2 * (a0 == 11 ? 11 : 22));
  assert(a3 == a0 + 3 * (a0 == 11 ? 11 : 22));
  assert(a4 == a0 + 4 * (a0 == 11 ? 11 : 22));
  assert(a5 == a0 + 5 * (a0 == 11 ? 11 : 22));
  assert(a6 == a0 + 6 * (a0 == 11 ? 11 : 22));
  return a6;
}
long alignment_check8(long a0, long a1, long a2, long a3, long a4, long a5, long a6, long a7) {
  assert(a0 == 11 || a0 == 23);
  assert(a1 == a0 + 1 * (a0 == 11 ? 11 : 22));
  assert(a2 == a0 + 2 * (a0 == 11 ? 11 : 22));
  assert(a3 == a0 + 3 * (a0 == 11 ? 11 : 22));
  assert(a4 == a0 + 4 * (a0 == 11 ? 11 : 22));
  assert(a5 == a0 + 5 * (a0 == 11 ? 11 : 22));
  assert(a6 == a0 + 6 * (a0 == 11 ? 11 : 22));
  assert(a7 == a0 + 7 * (a0 == 11 ? 11 : 22));
  return a7;
}
long alignment_check9(long a0, long a1, long a2, long a3, long a4, long a5, long a6, long a7, long a8) {
  assert(a0 == 11 || a0 == 23);
  assert(a1 == a0 + 1 * (a0 == 11 ? 11 : 22));
  assert(a2 == a0 + 2 * (a0 == 11 ? 11 : 22));
  assert(a3 == a0 + 3 * (a0 == 11 ? 11 : 22));
  assert(a4 == a0 + 4 * (a0 == 11 ? 11 : 22));
  assert(a5 == a0 + 5 * (a0 == 11 ? 11 : 22));
  assert(a6 == a0 + 6 * (a0 == 11 ? 11 : 22));
  assert(a7 == a0 + 7 * (a0 == 11 ? 11 : 22));
  assert(a8 == a0 + 8 * (a0 == 11 ? 11 : 22));
  return a8;
}
long alignment_check10(long a0, long a1, long a2, long a3, long a4, long a5, long a6, long a7, long a8, long a9) {
  assert(a0 == 11 || a0 == 23);
  assert(a1 == a0 + 1 * (a0 == 11 ? 11 : 22));
  assert(a2 == a0 + 2 * (a0 == 11 ? 11 : 22));
  assert(a3 == a0 + 3 * (a0 == 11 ? 11 : 22));
  assert(a4 == a0 + 4 * (a0 == 11 ? 11 : 22));
  assert(a5 == a0 + 5 * (a0 == 11 ? 11 : 22));
  assert(a6 == a0 + 6 * (a0 == 11 ? 11 : 22));
  assert(a7 == a0 + 7 * (a0 == 11 ? 11 : 22));
  assert(a8 == a0 + 8 * (a0 == 11 ? 11 : 22));
  assert(a9 == a0 + 9 * (a0 == 11 ? 11 : 22));
  return a9;
}
