/*----------------------------------------------------------------*
 *                         Strings                                *
 *----------------------------------------------------------------*/

/*
  A string is represented as a C-string prepended with the 
  string size (tagged).

  A char is represented as an integer (i.e., either as i or 2i+1 if
  tagging is enabled).

  Functions named ML are called from the ML Kit and handles tagging
  etc. Garbage collection may also involve certain restrictions, for
  instance, that a table is initialized before returning.
*/

#ifndef __STRING_H
#define __STRING_H

#include "Flags.h"

// Representation of ML strings

typedef struct stringDesc {
  unsigned int size;             // Size of string (tagged)
  unsigned char data;            // C String (null-terminated
} StringDesc;

#define sizeStringDefine(str)    ((((StringDesc *)str)->size) >> 6) /* Remove stringtag. We do not tag the size. */

/******************************************************************
 * EXTERNAL DECLARATIONS (ML functions, basislib)                 *
 ******************************************************************/

int chrCharML(int charNrML, int exn);
// int __bytetable_size(StringDesc *str);
// int __bytetable_sub(StringDesc *str, int i);
// void __bytetable_update(StringDesc *str, int i, int c);
void printStringML(StringDesc *str);
int lessStringML(StringDesc *str1, StringDesc *str2);
int lesseqStringML(StringDesc *str1, StringDesc *str2);
int greaterStringML(StringDesc *str1, StringDesc *str2);
int greatereqStringML(StringDesc *str1, StringDesc *str2);
int equalStringML(StringDesc *str1, StringDesc *str2);

#ifdef PROFILING
StringDesc *allocStringProfilingML(int rAddr, int sizeML, int pPoint);
StringDesc *concatStringProfilingML(int rAddr, StringDesc *str1, StringDesc *str2, int pPoint);
StringDesc *implodeCharsProfilingML (int rAddr, int xs, int pPoint);
StringDesc *implodeStringProfilingML(int rAddr, int xs, int pPoint);
StringDesc *convertStringToMLProfiling(int rAddr, unsigned char *cStr, int pPoint);
StringDesc *exnNameProfilingML(int rAddr, int e, int pPoint);
int *explodeStringProfilingML(int rAddr2, StringDesc *str, int pPoint);   // no region for the cons cells
#ifdef ENABLE_GC
StringDesc *copy_stringProf(int rAddr, StringDesc* s, int pPoint);
#endif
#else
StringDesc *allocStringML(int rAddr, int sizeML);
StringDesc *concatStringML(int rAddr, StringDesc *str1, StringDesc *str2);
StringDesc *implodeCharsML (int rAddr, int xs);
StringDesc *implodeStringML(int rAddr, int xs);
StringDesc *convertStringToML(int rAddr, unsigned char *cStr);
StringDesc* exnNameML(int rAddr, int e);
int *explodeStringML(int rAddr2, StringDesc *str);  // no region for the cons cells
#endif /*PROFILING*/

#endif /* __STRING_H */
