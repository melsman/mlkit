#include <stdio.h>
#include "Table.h"
#include "Tagging.h"

// word_sub0(t,i): extract element i from table t
/*Now in-lined
int 
word_sub0 (Table* t, int i)
{
  i = convertIntToC(i);
  return convertIntToML(*(&(t->data) + i));
}
*/

// word_update0(tablep,i,x): update element i in table t 
// with element x
/*Now in-lined
void 
word_update0 (Table* t, int i, int x)
{
  i = convertIntToC(i);
  x = convertIntToC(x);
  *(&(t->data) + i) = x;
  return;
}
*/

// word_table0(rAddr, n): return a pointer to a table 
// with n elements allocated in the region indicated by rAddr
Table* 
#ifdef PROFILING
word_table0Prof (int rAddr, int n, int pPoint)
#else
word_table0 (int rAddr, int n)
#endif
{
  Table* res;

  n = convertIntToC(n);
  #ifdef PROFILING
  res = (Table*)allocProfiling(rAddr, n+1, pPoint);
  #else
  res = (Table*)alloc(rAddr, n+1);
  #endif
  res->size = val_tag_table(n);
  #ifdef ENABLE_GC
  {
    int *p;
    int i;
    for ( i = 0, p = &(res->data) ; i < n ; i++, p++ )
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
Table* 
#ifdef PROFILING
word_table_initProf (int rAddr, int n, int x, int pPoint)
#else
word_table_init (int rAddr, int n, int x)
#endif
{
  Table* res;
  int i, *p;

  n = convertIntToC(n);
  #ifdef PROFILING
  res = (Table*)allocProfiling(rAddr, n+1, pPoint);
  #else
  res = (Table*)alloc(rAddr, n+1);
  #endif
  res->size = val_tag_table(n);

  p = &(res->data);
  for ( i = 0 ; i < n ; i ++ )
    {
      *p++ = x;
    }      
  return res;
}

// table_size(t) : returns the number of elements in t. This 
// function is needed by the polymorphic equality function 
// on vectors (EliminateEq.sml). 
/*Now in-lined
int 
table_size (Table *t)
{
  return convertIntToML(get_table_size(t->size));
}
*/
