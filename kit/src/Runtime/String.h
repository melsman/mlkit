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

#ifndef STRING_H
#define STRING_H

#include "Flags.h"
#include "Region.h"

// Representation of ML strings

typedef struct stringDesc {
  unsigned int size;             // Size of string (tagged)
  unsigned char data;            // C String (null-terminated
} StringDesc;

typedef StringDesc* String;

#define sizeStringDefine(str)    ((((String)(str))->size) >> 6) /* Remove stringtag. We do not tag the size. */

/******************************************************************
 * EXTERNAL DECLARATIONS (ML functions, basislib)                 *
 ******************************************************************/

int chrCharML(int charNrML, int exn);
// int __bytetable_size(String str);
// int __bytetable_sub(String str, int i);
// void __bytetable_update(String str, int i, int c);
void printStringML(String str);
int lessStringML(String str1, String str2);
int lesseqStringML(String str1, String str2);
int greaterStringML(String str1, String str2);
int greatereqStringML(String str1, String str2);
int equalStringML(String str1, String str2);

#ifdef PROFILING
String allocStringProfilingML(Region rAddr, int sizeML, int pPoint);
String concatStringProfilingML(Region rAddr, String str1, String str2, int pPoint);
String implodeCharsProfilingML (Region rAddr, int xs, int pPoint);
String implodeStringProfilingML(Region rAddr, int xs, int pPoint);
String convertStringToMLProfiling(Region rAddr, unsigned char *cStr, int pPoint);
String exnNameProfilingML(Region rAddr, int e, int pPoint);
int *explodeStringProfilingML(Region rAddr2, String str, int pPoint);   // no region for the cons cells
#else
String allocStringML(Region rAddr, int sizeML);
String concatStringML(Region rAddr, String str1, String str2);
String implodeCharsML (Region rAddr, int xs);
String implodeStringML(Region rAddr, int xs);
String convertStringToML(Region rAddr, unsigned char *cStr);
String exnNameML(Region rAddr, int e);
int *explodeStringML(Region rAddr2, String str);  // no region for the cons cells
#endif /*PROFILING*/

#endif /* STRING_H */
