
#ifndef __TABLE_H
#define __TABLE_H
#include "Region.h"

typedef struct table {
  int size;     // combined size and tag-field
  int data;     // first element
} Table;

// word_sub0(t,i): extract element i from table t; no bounds check
int word_sub0 (Table* t, int i);

// word_update0(tablep,i,x): update element i in table t 
// with element x; no bounds check
void word_update0 (Table* t, int i, int x);

// word_table0(rAddr, n): return a pointer to a table 
// with n elements allocated in the region indicated by rAddr
#ifdef PROFILING
Table* word_table0Prof (int rAddr, int n, int pPoint);
#else
Table* word_table0 (int rAddr, int n);
#endif

// word_table_init(rAddr, n, x): return a pointer to a table 
// with n initialized (=x) elements allocated in the 
// region indicated by rAddr
#ifdef PROFILING
Table* word_table_initProf (int rAddr, int n, int x, int pPoint);
#else
Table* word_table_init (int rAddr, int n, int x);
#endif

// table_size(t) : returns the number of elements in t. This 
// function is needed by the polymorphic equality function 
// on vectors (EliminateEq.sml). Also used for bounds 
// checking.
int table_size (Table * t);

// copy_table(rAddr,t): copy the table t into a new table 
// allocated in region rAddr.
#ifdef ENABLE_GC
#ifdef PROFILING
Table *copy_tableProf(int rAddr, Table *t, int pPoint);
#else
Table *copy_table(int rAddr, Table *t);
#endif 
#endif 

#endif /*__TABLE_H*/


