/*Table.h*/

#ifndef TABLE
#define TABLE
#include "Region.h"

/*With region inference you cannot allocate more consecutive
  memory than the size ALLOCATABLE_WORDS_IN_REGION_PAGE of a
  region page.  Therefore tables (i.e., sml vectors & arrays)
  are implemented as a tree of primitive tables.  A primitive
  table will fit in a region page.

  Table.c implements the following sml types & some operations
  on them

    datatype tree = Lf | Br of tree * prim_table * tree
    type table = int (*size in bytes*) * tree

  "Br (a00, pa, a01)" is represented as a pointer to a struct
  containing a pointer to a00 followed by a pointer to a01,
  followed by the prim_table.  "Lf" is represented as NULL.

  `pa' is a primitive table;
  `pi' is an index in a primitive table (an int);
  `path' is an int which, viewed as a binary number, represents a path in the
  binary tree that represents a table;
  `a' : table;
  `a0' : table0;
  `x' : ---an element in a table;
  `i' : int ---an index;
  `n' : int ---the number of elements in some table (which, then, has the
  legal indexes 0, ..., n-1);

  an index i into a0 : table0 may be thought of as a pair (path, pi)
  consisting of a path in the binary tree a0, and an index pi into a
  primitive table.  (path, pi) = (i div ALLOCATABLE_WORDS_IN_REGION_PAGE,
                                  i mod ALLOCATABLE_WORDS_IN_REGION_PAGE).
*/

#define ALLOCATABLE_WORDS_IN_PRIM_ARRAY (ALLOCATABLE_WORDS_IN_REGION_PAGE - 10)

/*I subtract 3 because we need two words for the pointers to a00
  and a01, and maybe one word for a tag.*/

/*types: primitive tables Prim_table, and tables Tree
  (i.e., trees of primitive tables):*/

typedef void * Prim_table [ALLOCATABLE_WORDS_IN_PRIM_ARRAY];

typedef struct tree {
  struct tree * child [2];
  Prim_table prim_table; } Tree;

typedef struct table {
  int table_size;
  Tree tree; } Table;

/*Functions that can be called by the compiled code (i.e., there
  is a prim "word_sub" in the prelude, etc.):

  byte_table0 (rAddr, n) & word_table0 (rAddr, n) = a pointer to
    a table with n elements allocated in the region indicated by
    rAddr, i.e., something of type Table.

  word_sub0, word_update0, byte_sub0 & byte_update0 are
    self-explanatory.

  table_size tablep = the number of elements in the table
    pointed to by "tablep".  This is needed by the polymorphic
    equality function on tables which is written in LambdaExp in
    EliminateEq.sml.*/

int    word_sub0 (int tablep, int i);
void   word_update0 (int tablep, int i, unsigned int x);
int    word_table0 (int rAddr, int words);
int    word_table0Prof (int rAddr, int words, int pPoint);
int    table_size (int tablep);

/*should not be used; charvectors and chararrays are now represented
  as strings 04/03/98-Martin

int    byte_sub0 (int tablep, int i);
void   byte_update0 (int tablep, int i, unsigned int x);
int    byte_table0 (int rAddr, int bytes);

*/

#endif /*TABLE*/


