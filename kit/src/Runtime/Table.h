/*Table.h*/

#ifndef __TABLE_H
#define __TABLE_H
#include "Region.h"

/*

  With region inference you cannot allocate more consecutive memory
  than the size ALLOCATABLE_WORDS_IN_REGION_PAGE of a region page.
  Therefore tables (i.e., sml vectors & arrays) are implemented as a
  tree of primitive tables.  A primitive table will fit in a region
  page. Notice, that CharVectors/Arrays and ByteVectors/Arrays are
  implemented as updateable strings.

  Table.c implements the following sml types & some operations
  on them

    datatype tree = Lf | Br of tree * prim_table * tree
    type table = int (*size in bytes*) * tree

  "Br (a00, pa, a01)" is represented as a pointer to a struct
  containing a pointer to a00 followed by a pointer to a01,
  followed by the prim_table.  "Lf" is represented as NULL.

  `pa'  : is a primitive table;
  `pi'  : is an index in a primitive table (an int);
  `path': is an int which, viewed as a binary number, represents a path in the
          binary tree that represents a table;
  `a'   : table;
  `a0'  : table0;
  `x'   : an element in a table;
  `i'   : int -- an index;
  `n'   : int -- the number of elements in some table (which, then, has the
                 legal indexes 0, ..., n-1);

  an index i into a0 : table0 may be thought of as a pair (path, pi)
  consisting of a path in the binary tree a0, and an index pi into a
  primitive table.  (path, pi) = (i div ALLOCATABLE_WORDS_IN_REGION_PAGE + 1,
                                  i mod ALLOCATABLE_WORDS_IN_REGION_PAGE).

  Notice, that we add one to the path, such that the left most bit in
  a path is always one, see function pa_entry in file Table.c

  A tree with primitive tables has the following layout:

                       +----+----+---------------------+
                       |...0|...1|prim table for path=1|
                       +----+----+---------------------+
                         |    |
                         |    +--------------------------------+
                        \|/                                   \|/
   +----+----+----------------------+         +----+----+----------------------+
   |..00|..10|prim table for path=10|	      |..01|..11|prim table for path=11|
   +----+----+----------------------+	      +----+----+----------------------+
      |   |                                     |     |
     \|/  +----------------------+             \|/    +------------+
    tree                         |             tree                |
    for                         \|/            for                \|/
    path=xxxx00                 tree           path=xxxx01         tree
                                for                                for
                                path=xxxx10                        path=xxxx11 

*/

/* Subtract 3 because we need two words for the pointers to a00
   and a01, and one word for the combined size and tag field.  
   2 extra for the objectdescriptor if profiling is enabled */

#ifdef PROFILING
#define ALLOCATABLE_WORDS_IN_PRIM_ARRAY (ALLOCATABLE_WORDS_IN_REGION_PAGE - 3 - 2)
#else
#define ALLOCATABLE_WORDS_IN_PRIM_ARRAY (ALLOCATABLE_WORDS_IN_REGION_PAGE - 3)
#endif

/* primitive tables Prim_table, and tables Tree
   (i.e., trees of primitive tables).           */
typedef void * Prim_table [ALLOCATABLE_WORDS_IN_PRIM_ARRAY];
typedef struct tree {
  struct tree * child [2];
  Prim_table prim_table; } Tree;

/* a table has the combined size and tag field together with
   the tree of primitive tables                              */      
typedef struct table {
  int table_size;
  Tree tree; } Table;

/* 

  The following functions can be called by the compiled code (i.e.,
  there is a prim "word_sub" in the prelude, etc.):

  word_table0(rAddr, n): a pointer to a table with n elements
                           allocated in the region indicated by rAddr,
                           (i.e., something of type Table).

  word_sub0(tablep,i): extract element i from table tablep.

  word_update0(tablep,i,x): update element i in table tablep with
                              element x

  table_size(tablep) : the number of elements in the table pointed to
                         by tablep. This is needed by the polymorphic
                         equality function on tables which is written in
                         LambdaExp in EliminateEq.sml.

  copy_table(rAddr,orig_tablep): copy the table orig_tablep into a 
                                   new table allocated in region rAddr.
*/

int    word_sub0 (int tablep, int i);
void   word_update0 (int tablep, int i, unsigned int x);
int    word_table0 (int rAddr, int words);
int    word_table0Prof (int rAddr, int words, int pPoint);
int    table_size (int tablep);
int    copy_table(int rAddr, int orig_tablep);

#ifdef PROFILING
int    copy_tableProf(int rAddr, int orig_tablep, int pPoint);
#endif 

#endif /*__TABLE_H*/


