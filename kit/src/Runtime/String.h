/*----------------------------------------------------------------*
 *                         Strings                                *
 *----------------------------------------------------------------*/

#ifndef STRING
#define STRING

/*----------------------------------------------------------------*
 * Include files                                                  *
 * Compiling: cc -Aa -c String.c                                  *
 *----------------------------------------------------------------*/
#include "Flags.h"

/* Representation of strings and stringfragments                  */
/* Same representation as in the runtime system by Lars Birkedal. */
/* The size of these structures, have to be a multiplum of words. */

typedef struct stringFragment {
  unsigned int fragmentSize;  /* Size of this fragment. */
  struct stringFragment * n;  /* Ptr. to next fragment in string. */
} StringFragment;

typedef struct stringDesc {
  unsigned int stringSize; /* Total size of string. */
  StringFragment sf;       /* StringDesc contains the first fragment and not a ptr. to it. */
} StringDesc;

#define sizeStringDefine(str)    ((((StringDesc *)str)->stringSize) >> 3) /* Remove stringtag. We do not tag the size. */

#define C_STRING_LENGTH 4096    /* Maximal length of C-string. */


/*----------------------------------------------------------------*
 *        Prototypes for external and internal functions.         *
 *----------------------------------------------------------------*/
extern char cString[C_STRING_LENGTH];

StringDesc *makeChar(int rAddr, char ch);
int chrChar (int charNr, int exn);
int sizeString(StringDesc *str);
int equalString (StringDesc *str1, StringDesc *str2);
int lessString (StringDesc *str1, StringDesc *str2);
int lesseqString (StringDesc *str1, StringDesc *str2);
int greaterString (StringDesc *str1, StringDesc *str2);
int greatereqString (StringDesc *str1, StringDesc *str2);
StringDesc *concatString(int rAddr, StringDesc *str1, StringDesc *str2);
void printString(StringDesc *str);
StringDesc *implodeChars (int rAddr, int xs);
StringDesc *implodeString(int rAddr, int xs);
int subString(StringDesc *str, int i);
void updateString(StringDesc *str, int i, int c);
StringDesc* allocString(int rAddr, int sz);
StringDesc* exnName(int rAddr, int e);

#if UNBOX_LISTS
int *explodeString (int rAddr2, StringDesc *str);  /* no region for the cons cells */
#else
int *explodeString (int rAddr1, int rAddr2, StringDesc *str);
#endif

void printString(StringDesc *str);
void printStringList(int xs);


/*Converting functions. */
void convertStringToC(StringDesc *mlStr, char *cStr, int cStrLen, int exn);
StringDesc *convertStringToML(int rAddr, char *cStr);

#ifdef PROFILING
/* Profiling functions. */
StringDesc *implodeCharsProfiling (int rAddr, int xs, int pPoint);
StringDesc *implodeStringProfiling(int rAddr, int xs, int pPoint);
StringDesc *convertStringToMLProfiling(int rAddr, char *cStr, int pPoint);
StringDesc *allocStringProfiling(int rAddr, int sz, int pPoint);
StringDesc *exnNameProfiling(int rAddr, int e, int pPoint);

#if UNBOX_LISTS
int *explodeStringProfiling (int rAddr2, StringDesc *str, int pPoint); /* no region for the cons cells */
#else
int *explodeStringProfiling (int rAddr1, int rAddr2, StringDesc *str, int pPoint);
#endif

StringDesc *concatStringProfiling(int rAddr, StringDesc *str1, StringDesc *str2, int pPoint);
#endif /*PROFILING*/

#endif /*STRING*/
