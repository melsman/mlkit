/*Table.c*/

/* There are some comments in Table.h. */

#include <stdio.h>
#include "Table.h"
#include "Tagging.h"

#define mod2(x) ((x) & 1)
#define div2(x) ((x) >> 1)

/* pa_entry(treep, path): a pointer to the primitive table
                          indicated by path. */
Prim_table *pa_entry(Tree *treep, unsigned int path) {
  return (path == 1) ?
    &(treep->prim_table) :
    pa_entry (treep->child [mod2 (path)], div2 (path));
}

/*
   A short note on div(/) and mod (%) in C:

   We have 
     i div ALLOCATABLE_WORDS_IN_PRIM_ARRAY + 1   =  
     i  /  ALLOCATABLE_WORDS_IN_PRIM_ARRAY + 1
   because
     i>=0 and ALLOCATABLE_WORDS_IN_PRIM_ARRAY>0.

   We have
     i mod ALLOCATABLE_WORDS_IN_PRIM_ARRAY       =
     i  %  ALLOCATABLE_WORDS_IN_PRIM_ARRAY 
   because
    ``If both operands are non-negative, then the result 
      is non-negative and smaller than the divisor'' 
    (Kernighan & Ritchie).
*/

/* word_entry(tablep, i): a pointer to the i'th entry in the
                          word table pointed to by tablep */
unsigned int *word_entry(int tablep, unsigned int iC) {
  unsigned int path, pi;
  void * pa_ref;
  path = iC / ALLOCATABLE_WORDS_IN_PRIM_ARRAY + 1;
  pi =   iC % ALLOCATABLE_WORDS_IN_PRIM_ARRAY;
  pa_ref = pa_entry (&(((Table *) tablep)->tree), path);
  /*  printf("path: %d, pi: %d\n", path, pi); 2001-05-12, Niels */
  return ((unsigned int *)pa_ref) + pi; 
}

/* word_sub0(tablep,i): extract element i from table tablep. */
int word_sub0(int tablep, int iML) {
  unsigned int * word_ref;
  word_ref = word_entry (tablep, convertIntToC(iML));
  return *word_ref; 
}

/* word_update0(tablep,i,x): update element i in table tablep with
                             element x */
void word_update0(int tablep, int iML, unsigned int x) {
  unsigned int * word_ref;

  /*  printf("enter word_update with iML = %d\n", iML); 2001-05-12, Niels */
  word_ref = word_entry (tablep, convertIntToC(iML));
  /*  printf("word_ref: %d and *word_ref: %d\n", word_ref, 42); */
  *word_ref = x; 
  /*  printf("leave word_update with iML = %d\n", iML); 2001-05-12, Niels */
}

/* table00(rAddr, path, next, m): table00 returns a Tree.
     rAddr is, of course, the region for the table; path is the
     path to the primitive table we are ``currently'' working on.
     m is the number of primitive tables needed. We initialize the
     table if GC is enabled. */
Tree *table00(int rAddr,unsigned int path,unsigned int next,unsigned int m) {
#ifdef ENABLE_GC
  int i;
#endif /* ENABLE_GC */
  if (path > m) 
    return NULL;
  else { 
    int *a0 = NULL;
    a0 = alloc(rAddr, sizeof(Tree) / 4);
    /* Beware: "/ 4" only works under the assumption that "sizeof (Tree)"
       is a multiple of 4 */
    ((Tree *) a0)->child [0] = table00 (rAddr, path + next    , next * 2, m);
    ((Tree *) a0)->child [1] = table00 (rAddr, path + next * 2, next * 2, m);
#ifdef ENABLE_GC
    for (i=0;i<ALLOCATABLE_WORDS_IN_PRIM_ARRAY;i++)
      *(((int *)((Tree *)a0)->prim_table)+i) = mlUNIT; /* Init to a scalar value */
#endif /* ENABLE_GC */
    return (Tree *) a0; 
  }
}

/* table0 (rAddr, words) = a pointer to a table with "words"
     words allocated in the region indicated by rAddr, (i.e.,
     something of type table). The "0" is there to indicate that
     table0 does not check the size bound (and raise Size). The
     table is initialised if GC is enabled. table0 uses the 
     recursive table00 to build the Tree of Prim_table's. 
     Take care: table0 does not set the table_size field 
     in the Table.

     The number m of primitive tables needed for a table of
     "words" words is not 
       "words div ALLOCATABLE_WORDS_IN_PRIM_ARRAY" 
     (in ml), rather it is
       "~(~words div ALLOCATABLE_WORDS_IN_PRIM_ARRAY)".
     Supposedly, this is the same as 
       "1 - ((1-words) / ALLOCATABLE_WORDS_IN_PRIM_ARRAY)" 
     (in c).

     The first call to table00 has been unfolded below because we want
     the first allocation to be an allocation of a "Table" rather than
     a "Tree": a "Table" is not simply a "Tree", it is a "Tree" with a
     combined tag and size field. */
Table *table0(int rAddr, int wordsC) {
#ifdef ENABLE_GC
  int i;
#endif /* ENABLE_GC */
  unsigned int path = 1;
  unsigned int next = 1;
  unsigned int m = 1 - ((1 - wordsC) / ALLOCATABLE_WORDS_IN_PRIM_ARRAY);
  int * tablep = NULL;
  Tree * treep = NULL;
  tablep = alloc(rAddr, sizeof(Table) / 4);
  /* Beware: "/ 4" only works under the assumption that "sizeof
     (Table)" is a multiple of 4 */
  treep = &(((Table *) tablep)->tree);
  treep->child [0] = table00 (rAddr, path + next    , next * 2, m); 
  treep->child [1] = table00 (rAddr, path + next * 2, next * 2, m);
#ifdef ENABLE_GC
    for (i=0;i<ALLOCATABLE_WORDS_IN_PRIM_ARRAY;i++)
      *(((int *)treep->prim_table)+i) = mlUNIT; /* Init to a scalar value */
#endif /* ENABLE_GC */
  return (Table *) tablep; 
}


/* word_table0(rAddr, n): a pointer to a table with n elements
                          allocated in the region indicated by rAddr,
                          (i.e., something of type Table). */
int word_table0(int rAddr, int wordsML) {
  int wordsC = convertIntToC(wordsML);
  Table * tablep = table0 (rAddr, wordsC);
  tablep->table_size = val_tag_table(wordsC);
  return (int) tablep; 
}

/* table_size(tablep) : the number of elements in the table pointed to
                        by tablep. This is needed by the polymorphic
                        equality function on tables which is written
                        in LambdaExp in EliminateEq.sml. */
int table_size(int tablep) {
  int tab_sizeC = get_table_size(((Table *) tablep)->table_size);
  return convertIntToML (tab_sizeC);
}

/***********************************************************/
/* GARBAGE COLLECTION needs a function to evacuate a table */
/***********************************************************/

/* table00CP: same as table00 except the table is initialized */
/*            with the content of another primitive table.    */
Tree *table00CP(int rAddr,unsigned int path,unsigned int next,Tree *orig_tree) {
  int i;
  if (orig_tree == NULL)
    return NULL;
  else { 
    int *a0 = NULL;
    a0 = alloc(rAddr, sizeof(Tree) / 4);
    /* Beware: "/ 4" only works under the assumption that "sizeof (Tree)"
       is a multiple of 4 */
    ((Tree *) a0)->child [0] = table00CP(rAddr, path + next    , next * 2, orig_tree->child[0]);
    ((Tree *) a0)->child [1] = table00CP(rAddr, path + next * 2, next * 2, orig_tree->child[1]);
    for (i=0;i<ALLOCATABLE_WORDS_IN_PRIM_ARRAY;i++)
      *(((int *)((Tree *)a0)->prim_table)+i) = *(((int *)orig_tree->prim_table)+i);
    return (Tree *) a0; 
  }
}

/* copy_table: same as table0 and word_table0 above except we copy
               from another table. */
int copy_table(int rAddr, int orig_tablep) {
  int i;
  unsigned int path = 1;
  unsigned int next = 1;
  int *tablep = NULL;
  Tree *treep = NULL;
  Tree *orig_treep = &(((Table *)orig_tablep)->tree);
  /* printf("enter copy_table\n");*/  /* Debug 2001-01-13, Niels */

  tablep = alloc(rAddr, sizeof(Table) / 4);
  treep = &(((Table *)tablep)->tree);
  treep->child [0] = table00CP(rAddr, path + next    , next * 2, orig_treep->child[0]);
  treep->child [1] = table00CP(rAddr, path + next * 2, next * 2, orig_treep->child[1]);
  for (i=0;i<ALLOCATABLE_WORDS_IN_PRIM_ARRAY;i++)
    *(((int *)treep->prim_table)+i) = *(((int *)orig_treep->prim_table)+i);
  ((Table *)tablep)->table_size = ((Table *)orig_tablep)->table_size;
  /* printf("leaving copy_table\n");*/  /* Debug 2001-01-13, Niels */
  return (int)tablep; 
}

/*****************************************************************/
/* PROFILING VARIANT OF word_table0, and its auxiliary functions */
/*****************************************************************/
#ifdef PROFILING
Tree *table00Prof (int rAddr, unsigned int path, unsigned int next, unsigned int m, int pPoint) {
  /* printf("table00Prof: path = %d; next = %d; pPoint = %d\n",path,next,pPoint);*/
#ifdef ENABLE_GC
  int i;
#endif /* ENABLE_GC */
  if (path > m) { return NULL; }
  else { 
    int * a0 = NULL;
    /*printf("table00Prof: sizeof (Tree) = %d\n", sizeof(Tree)); mads*/
    a0 = allocProfiling(rAddr, sizeof(Tree) / 4, pPoint);
    /*    allocRecordMLProf (rAddr, sizeof (Tree) / 4, a0, pPoint); 12/04/1999, Niels */
    /*Beware: "/ 4" only works under the assumption that "sizeof (Tree)"
      is a multiple of 4*/
    ((Tree *) a0)->child [0] = table00Prof (rAddr, path + next    , next * 2, m, pPoint);
    ((Tree *) a0)->child [1] = table00Prof (rAddr, path + next * 2, next * 2, m, pPoint);
#ifdef ENABLE_GC
    for (i=0;i<ALLOCATABLE_WORDS_IN_PRIM_ARRAY;i++)
      *(((int *)((Tree *)a0)->prim_table)+i) = mlUNIT; /* Init to a scalar value */
#endif /* ENABLE_GC */
    return (Tree *) a0; }}

Table *
table0Prof (int rAddr, int wordsC, int pPoint) {
  /*The number m of primitive tables needed for a table of
    "words" words is not "words div
    ALLOCATABLE_WORDS_IN_PRIM_ARRAY" (in ml), rather it is
    "~(~words div ALLOCATABLE_WORDS_IN_PRIM_ARRAY)".
    Supposedly, this is the same as "1 - ((1-words) /
    ALLOCATABLE_WORDS_IN_PRIM_ARRAY)" (in c).

    The first call to table00 has been unfolded below because we
    want the first allocation to be an allocation of a "Table"
    rather than a "Tree": a "Table" is not simply a "Tree", it
    is a "Tree" with a size.

    Take care: table0 does not set the table_size field in the
    Table.*/
#ifdef ENABLE_GC
  int i;
#endif /* ENABLE_GC */
  unsigned int path = 1;
  unsigned int next = 1;
  unsigned int m = 1 - ((1 - wordsC) / ALLOCATABLE_WORDS_IN_PRIM_ARRAY);
  int * tablep = NULL;
  Tree * treep = NULL;
  /*printf("table0Prof: sizeof (Table) = %d; number of primitive tables wanted: %d\n", sizeof(Table),m); mads*/
  tablep = allocProfiling(rAddr, sizeof(Table) / 4, pPoint);
  /*  allocRecordMLProf (rAddr, sizeof (Table) / 4, tablep, pPoint); 12/04/1999, Niels */
  /*Beware: "/ 4" only works under the assumption that "sizeof
    (Table)" is a multiple of 4*/
  treep = &(((Table *) tablep)->tree);
  treep->child [0] = table00Prof (rAddr, path + next    , next * 2, m, pPoint);
  treep->child [1] = table00Prof (rAddr, path + next * 2, next * 2, m, pPoint);
#ifdef ENABLE_GC
    for (i=0;i<ALLOCATABLE_WORDS_IN_PRIM_ARRAY;i++)
      *(((int *)treep->prim_table)+i) = mlUNIT; /* Init to a scalar value */
#endif /* ENABLE_GC */
  return (Table *) tablep; }

int word_table0Prof (int rAddr, int wordsML, int pPoint) {
  Table * tablep ;
  int wordsC = convertIntToC(wordsML);
  /*  printf("ENTER word_table0Prof (%d words)\n", wordsC);
      printf("ALLOCATABLE_WORDS_IN_PRIM_ARRAY = %d\n",ALLOCATABLE_WORDS_IN_PRIM_ARRAY); */
  tablep= table0Prof (rAddr, wordsC, pPoint);
  tablep->table_size = val_tag_table(wordsC);
  /*   printf("LEAVE word_table0Prof (%d words)\n", wordsC);*/
  return (int) tablep; }

/*************************************************************************/
/* GARBAGE COLLECTION and PROFILING needs a function to evacuate a table */
/*************************************************************************/

/* table00CP: same as table00 except the table is initialized */
/*            with the content of another primitive table.    */
Tree *table00CPProf(int rAddr,unsigned int path,unsigned int next,Tree *orig_tree, int pPoint) {
  int i;
  if (orig_tree == NULL)
    return NULL;
  else { 
    int *a0 = NULL;
    a0 = allocProfiling(rAddr, sizeof(Tree) / 4, pPoint);
    /* Beware: "/ 4" only works under the assumption that "sizeof (Tree)"
       is a multiple of 4 */
    ((Tree *) a0)->child [0] = table00CPProf(rAddr, path + next    , next * 2, orig_tree->child[0], pPoint);
    ((Tree *) a0)->child [1] = table00CPProf(rAddr, path + next * 2, next * 2, orig_tree->child[1], pPoint);
    for (i=0;i<ALLOCATABLE_WORDS_IN_PRIM_ARRAY;i++)
      *(((int *)((Tree *)a0)->prim_table)+i) = *(((int *)orig_tree->prim_table)+i);
    return (Tree *) a0; 
  }
}

/* copy_table: same as table0 and word_table0 above except we copy
               from another table. */
int copy_tableProf(int rAddr, int orig_tablep, int pPoint) {
  int i;
  unsigned int path = 1;
  unsigned int next = 1;
  int *tablep = NULL;
  Tree *treep = NULL;
  Tree *orig_treep = &(((Table *)orig_tablep)->tree);
  /* printf("enter copy_table\n");*/  /* Debug 2001-01-13, Niels */

  tablep = allocProfiling(rAddr, sizeof(Table) / 4, pPoint);
  treep = &(((Table *)tablep)->tree);
  treep->child [0] = table00CPProf(rAddr, path + next    , next * 2, orig_treep->child[0], pPoint);
  treep->child [1] = table00CPProf(rAddr, path + next * 2, next * 2, orig_treep->child[1], pPoint);
  for (i=0;i<ALLOCATABLE_WORDS_IN_PRIM_ARRAY;i++)
    *(((int *)treep->prim_table)+i) = *(((int *)orig_treep->prim_table)+i);
  ((Table *)tablep)->table_size = ((Table *)orig_tablep)->table_size;
  /* printf("leaving copy_table\n");*/  /* Debug 2001-01-13, Niels */
  return (int)tablep; 
}

#endif /* Profiling */


