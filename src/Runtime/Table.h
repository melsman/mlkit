#ifndef __TABLE_H
#define __TABLE_H
#include "Region.h"
#include "Tagging.h"

typedef struct {
  size_t size;     // combined size and tag-field
  size_t data[];   // first element
} TableDesc;

typedef TableDesc* Table;

// word_table0(rAddr, n): return a pointer to a table
// with n elements allocated in the region indicated by rAddr
Table REG_POLY_FUN_HDR(word_table0, Region rAddr, size_t n);

// word_table_init(rAddr, n, x): return a pointer to a table
// with n initialized (=x) elements allocated in the
// region indicated by rAddr
Table REG_POLY_FUN_HDR(word_table_init, Region rAddr, size_t n, size_t x);

#endif /*__TABLE_H*/
