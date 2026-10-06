#include <stddef.h>

#include "Table.h"
extern uintptr_t ap_callback(uintptr_t);
Table REG_POLY_FUN_HDR(ap_foreign, Region r, uintptr_t n) {
  Table first = REG_POLY_CALL(word_table0,r,n);
  (void)ap_callback(n);
  (void)REG_POLY_CALL(word_table0,r,n);
  return first;
}
