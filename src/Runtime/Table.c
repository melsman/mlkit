/*Table.c*/
/*There are some comments in Table.h.

  Beware: If you change anything in the representation of
  tables, remember also to change the treatment of string
  constants in KbpToHpPa.sml (fun generateString) and
  KbpToC.sml*/

#include <stdio.h>
#include "Table.h"
#include "Tagging.h"
#define mod2(x) ((x) & 1)
#define div2(x) ((x) >> 1)


/*pa_entry (treep, path) = a pointer to the primitive table
  indicated by path*/
Prim_table *
pa_entry (Tree * treep, unsigned int path) {
  return (path == 1) ?
    &(treep->prim_table) :
    pa_entry (treep->child [mod2 (path)], div2 (path)) ;}


/*word_entry (tablep, i) = a pointer to the i'th entry in the
  word table pointed to by tablep*/
unsigned int *
word_entry (int tablep, unsigned int i) {
  unsigned int path, pi;
  void * pa_ref;
  i = (unsigned int) convertIntToC (i);
  path = i / ALLOCATABLE_WORDS_IN_PRIM_ARRAY + 1;
  pi =   i % ALLOCATABLE_WORDS_IN_PRIM_ARRAY;
  pa_ref = pa_entry (&(((Table *) tablep)->tree), path);
  return ((unsigned int *)pa_ref) + pi; }

/*
   i div ALLOCATABLE_WORDS_IN_PRIM_ARRAY + 1   =  
   i  /  ALLOCATABLE_WORDS_IN_PRIM_ARRAY + 1
fordi i>=0 /\ ALLOCATABLE_WORDS_IN_PRIM_ARRAY>0

   i mod ALLOCATABLE_WORDS_IN_PRIM_ARRAY       =
   i  %  ALLOCATABLE_WORDS_IN_PRIM_ARRAY 
fordi ``If both operands are non-negative, then the result is non-negative
and smaller than the divisor'' (Kernighan & Ritchie), & x mod y er i Math.c 
defineret

   if ( (x > 0 && y > 0) || (x < 0 && y < 0) || (x % y == 0) ) return x % y;
    return (x % y) + y;

eller også er det fordi 0 % x altid er 0.  (Det eneste, jeg ikke helt kan 
finde ud af, er, hvad der sker, når x=0.  y=0 kan ikke forekomme, så hvis 
ikke x=0, er begge >0, & så er vi i situation 1 i if'en ovenfor.)
*/



/*word_sub0 & word_update0 are called by the compiled code,
  i.e., there is a prim "word_sub" in the prelude, etc.*/
int
word_sub0 (int tablep, int i) {
  unsigned int * word_ref;
  word_ref = word_entry (tablep, i);
  return *word_ref; }

void
word_update0 (int tablep, int i, unsigned int x) {
  unsigned int * word_ref;
  word_ref = word_entry (tablep, i);
  *word_ref = x; }

/*byte_entry, byte_sub & byte_update are very much like their
  word_-counterparts.  The difference is the `* 4' and some type
  casts are `unsigned char' rather than `unsigned int'.*/

unsigned char *
byte_entry (int tablep, int i) {
  unsigned int path, pi;
  void * pa_ref;
  i = (unsigned int) convertIntToC (i);
  path = i / (ALLOCATABLE_WORDS_IN_PRIM_ARRAY * 4) + 1;
  pi =   i % (ALLOCATABLE_WORDS_IN_PRIM_ARRAY * 4);
  pa_ref = pa_entry (&(((Table *) tablep)->tree), path);
  return ((unsigned char *)pa_ref) + pi; }

int
byte_sub0 (int tablep, int i) {
  unsigned char * byte_ref;
  byte_ref = byte_entry (tablep, i);
  return *byte_ref; }

void
byte_update0 (int tablep, int i, unsigned int x) {
  unsigned char * byte_ref;
  byte_ref = byte_entry (tablep, i);
  *byte_ref = (unsigned char) x; }


/*byte_table0 (rAddr, n) & word_table0 (rAddr, n) = a pointer to
    a table with n elements allocated in the region indicated by
    rAddr, i.e., something of type Table.

  table0 (rAddr, words) = a pointer to a table with "words"
    words allocated in the region indicated by rAddr, i.e.,
    something of type table (the "0" is there to indicate that
    table0 does not check the size bound (and raise Size)).  The
    table is not initialised.  table0 uses the recursive table00
    to build the Tree of Prim_table's.  Take care: table0 does
    not set the table_size field in the Table.

  table00 (rAddr, path, next, m): table00 returns a Tree.
    rAddr is, of course, the region for the table; path is the
    path to the primitive table we are ``currently'' working on.
    m is the number og primitive tables needed.*/

Tree *
table00 (int rAddr, unsigned int path, unsigned int next, unsigned int m) {
  if (path > m) { return NULL; }
  else { 
    int * a0 = NULL;
    allocRecordML (rAddr, sizeof (Tree) / 4, a0);
    /*Beware: "/ 4" only works under the assumption that "sizeof (Tree)"
      is a multiple of 4*/
    ((Tree *) a0)->child [0] = table00 (rAddr, path + next    , next * 2, m);
    ((Tree *) a0)->child [1] = table00 (rAddr, path + next * 2, next * 2, m);
    return (Tree *) a0; }}

Table *
table0 (int rAddr, int words) {
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
  unsigned int path = 1;
  unsigned int next = 1;
  unsigned int m = 1 - ((1 - convertIntToC (words)) / ALLOCATABLE_WORDS_IN_PRIM_ARRAY);
  int * tablep = NULL;
  Tree * treep = NULL;
  allocRecordML (rAddr, sizeof (Table) / 4, tablep);
  /*Beware: "/ 4" only works under the assumption that "sizeof
    (Table)" is a multiple of 4*/
  treep = &(((Table *) tablep)->tree);
  treep->child [0] = table00 (rAddr, path + next    , next * 2, m);
  treep->child [1] = table00 (rAddr, path + next * 2, next * 2, m);
  return (Table *) tablep; }


int
byte_table0 (int rAddr, int bytes) {
  int cbytes = convertIntToC (bytes);
  Table * tablep = table0 (rAddr, 1 - ((1 - convertIntToC (cbytes)) / 4)); 
  tablep->table_size = cbytes;
  return (int) tablep; }


int
word_table0 (int rAddr, int words) {
  Table * tablep = table0 (rAddr, words);
  tablep->table_size = words;
  return (int) tablep; }


int
table_size (int tablep) {
  return convertIntToML (((Table *) tablep)->table_size); }

/*****************************************************************/
/* PROFILING VARIANT OF word_table0, and its auxiliary functions */
/*****************************************************************/
#ifdef PROFILING

Tree *
table00Prof (int rAddr, unsigned int path, unsigned int next, unsigned int m, int pPoint) {
  /* printf("table00Prof: path = %d; next = %d; pPoint = %d\n",path,next,pPoint);*/
  if (path > m) { return NULL; }
  else { 
    int * a0 = NULL;
    /*printf("table00Prof: sizeof (Tree) = %d\n", sizeof(Tree)); mads*/
    allocRecordMLProf (rAddr, sizeof (Tree) / 4, a0, pPoint);
    /*Beware: "/ 4" only works under the assumption that "sizeof (Tree)"
      is a multiple of 4*/
    ((Tree *) a0)->child [0] = table00Prof (rAddr, path + next    , next * 2, m, pPoint);
    ((Tree *) a0)->child [1] = table00Prof (rAddr, path + next * 2, next * 2, m, pPoint);
    return (Tree *) a0; }}

Table *
table0Prof (int rAddr, int words, int pPoint) {
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
  unsigned int path = 1;
  unsigned int next = 1;
  unsigned int m = 1 - ((1 - convertIntToC (words)) / ALLOCATABLE_WORDS_IN_PRIM_ARRAY);
  int * tablep = NULL;
  Tree * treep = NULL;
  /*printf("table0Prof: sizeof (Table) = %d; number of primitive tables wanted: %d\n", sizeof(Table),m); mads*/
  allocRecordMLProf (rAddr, sizeof (Table) / 4, tablep, pPoint);
  /*Beware: "/ 4" only works under the assumption that "sizeof
    (Table)" is a multiple of 4*/
  treep = &(((Table *) tablep)->tree);
  treep->child [0] = table00Prof (rAddr, path + next    , next * 2, m, pPoint);
  treep->child [1] = table00Prof (rAddr, path + next * 2, next * 2, m, pPoint);
  return (Table *) tablep; }


int
byte_table0Prof (int rAddr, int bytes, int pPoint) {
  int cbytes = convertIntToC (bytes);
  Table * tablep = table0Prof (rAddr, 1 - ((1 - convertIntToC (cbytes)) / 4), pPoint); 
  tablep->table_size = cbytes;
  return (int) tablep; }

int
word_table0Prof (int rAddr, int words, int pPoint) {
  Table * tablep ;
  /*printf("ENTER word_table0Prof (%d words)\n", words);
  printf("ALLOCATABLE_WORDS_IN_PRIM_ARRAY = %d\n",ALLOCATABLE_WORDS_IN_PRIM_ARRAY); mads*/
  tablep= table0Prof (rAddr, words, pPoint);
  tablep->table_size = words;
  /* printf("LEAVE word_table0Prof (%d words)\n", words);*/
  return (int) tablep; }

#endif


/*TODO:

~ (  ~n    div y)   svarer ifl. def. af divInt i Math.c til 
- (((-n+1)  /  y) -   1), som er lig med (tror jeg)
- ((( 1-n)  /  y) -   1), som er lig med (tror jeg)
  -(( 1-n)  /  y) - (-1), som er lig med (tror jeg)
  -(( 1-n)  /  y) +   1,  som er lig med (tror jeg)
1  -  ((1-n)  /  y).

ovf. står `/' for c-division.


skal i være/cast'es til unsigned?

indekser +/- 1 ?

*/



/*prim_alloc_words_for_array (rAddr, n) = a pointer to an array 
  with n words allocated in the region indicated by rAddr.
  1<=n<= (eller: <?) ALLOCATABLE_WORDS_IN_REGION_PAGE*/
/*KILL: 11/02/1998 12:45. tho.
int *
prim_alloc_words (int rAddr, int n) {
  int* p = NULL;
  allocRecordML (rAddr, convertIntToC (n), p);
  return p; }

unsigned int
prim_byte_sub (int a, int i) {
  return (unsigned int) ((unsigned char *) a) [convertIntToC (i)]; }

void
prim_byte_update (int a, int i, unsigned int x) {
  ((unsigned char *) a) [convertIntToC (i)] = (unsigned char) x; }


int
prim_word_sub (int a, int i) {
  return ((unsigned int *) a) [convertIntToC (i)]; }

void
prim_word_update (int a, int i, unsigned int x) {
  ((unsigned int *) a) [convertIntToC (i)] = x; }

*/

