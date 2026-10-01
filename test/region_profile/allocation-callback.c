#include <stddef.h>

#include "Table.h"
extern uintptr_t ap_callback(uintptr_t);
Table ap_foreign(Region r, uintptr_t n) {
  Table first = word_table0(r,n);
  (void)ap_callback(n);
  (void)word_table0(r,n);
  return first;
}
