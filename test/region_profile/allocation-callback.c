#include <stddef.h>
#include <assert.h>

#include "Table.h"
extern uintptr_t ap_callback(uintptr_t);
Table REG_POLY_FUN_HDR(ap_foreign, Region r, uintptr_t n) {
  Table first = REG_POLY_CALL(word_table0,r,n);
  (void)ap_callback(n);
  (void)REG_POLY_CALL(word_table0,r,n);
  return first;
}

/* The site token is a stack argument on both supported ABIs. */
Table REG_POLY_FUN_HDR(ap_foreign_wide, Region r, uintptr_t n,
                      uintptr_t a, uintptr_t b, uintptr_t c, uintptr_t d,
                      uintptr_t e, uintptr_t f, uintptr_t g, uintptr_t h) {
  assert(a == n && b == n && c == n && d == n);
  assert(e == n && f == n && g == n && h == n);
  return REG_POLY_CALL(ap_foreign,r,n);
}
