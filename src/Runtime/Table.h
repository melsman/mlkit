
#ifndef __TABLE_H
#define __TABLE_H
#include "Region.h"

typedef struct {
  int size;     // combined size and tag-field
  int data;     // first element
} TableDesc;

typedef TableDesc* Table; 

// word_table0(rAddr, n): return a pointer to a table 
// with n elements allocated in the region indicated by rAddr
#ifdef PROFILING
Table word_table0Prof (Region rAddr, int n, int pPoint);
#else
Table word_table0 (Region rAddr, int n);
#endif

// word_table_init(rAddr, n, x): return a pointer to a table 
// with n initialized (=x) elements allocated in the 
// region indicated by rAddr
#ifdef PROFILING
Table word_table_initProf (Region rAddr, int n, int x, int pPoint);
#else
Table word_table_init (Region rAddr, int n, int x);
#endif

#endif /*__TABLE_H*/


