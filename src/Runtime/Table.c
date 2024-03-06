#include <stdio.h>
#include "Table.h"
#include "Tagging.h"

// word_table0(rAddr, n): return a pointer to a table
// with n elements allocated in the region indicated by rAddr
Table
REG_POLY_FUN_HDR(word_table0, Region rAddr, size_t n)
{
  Table res;

  n = convertIntToC(n);
  #ifdef PROFILING
  res = (Table)allocProfiling(rAddr, n+1, pPoint);
  #else
  res = (Table)alloc(rAddr, n+1);
  #endif
  res->size = val_tag_table(n);
  #ifdef ENABLE_GC
  {
    size_t *p;
    size_t i;
    for ( i = 0, p = res->data ; i < n ; i++, p++ )
      {
	*p = 1;     // scalar value
      }
  }
  #endif
  return res;
}

// word_table_init(rAddr, n, x): return a pointer to a table
// with n initialized (=x) elements allocated in the region
// indicated by rAddr
/* 'a */
Table
REG_POLY_FUN_HDR(word_table_init, Region rAddr, size_t n, size_t x)
{
  Table res;
  size_t i, *p;

  n = convertIntToC(n);
  #ifdef PROFILING
  res = (Table)allocProfiling(rAddr, n+1, pPoint);
  #else
  res = (Table)alloc(rAddr, n+1);
  #endif
  res->size = val_tag_table(n);

  p = res->data;
  for ( i = 0 ; i < n ; i ++ )
    {
      *p++ = x;
    }
  return res;
}
