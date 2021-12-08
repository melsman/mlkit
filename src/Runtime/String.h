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

#include <stdlib.h>
#include "Flags.h"
#include "Region.h"
#include "Tagging.h"

// Representation of ML strings

typedef struct stringDesc {
  size_t size;             // Size of string (tagged)
  char data;               // C String (null-terminated)
} StringDesc;

typedef StringDesc* String;

#define sizeStringDefine(str)    ((((String)(str))->size) >> 6) /* Remove stringtag. We do not tag the size. */

void convertStringToC(Context ctx, String mlStr, char *buf, size_t buflen, uintptr_t exn);

/******************************************************************
 * EXTERNAL DECLARATIONS (ML functions, basislib)                 *
 ******************************************************************/

void printStringML(String str);
size_t lessStringML(String str1, String str2);
size_t lesseqStringML(String str1, String str2);
size_t greaterStringML(String str1, String str2);
size_t greatereqStringML(String str1, String str2);
size_t equalStringML(String str1, String str2);

String REG_POLY_FUN_HDR(allocStringML, Region rAddr, size_t sizeML);
String REG_POLY_FUN_HDR(allocStringC, Region rAddr, size_t sizeC);
String REG_POLY_FUN_HDR(concatStringML, Region rAddr, String str1, String str2);
String REG_POLY_FUN_HDR(implodeCharsML, Region rAddr, size_t xs);
String REG_POLY_FUN_HDR(implodeStringML, Region rAddr, size_t xs);
String REG_POLY_FUN_HDR(convertStringToML, Region rAddr, const char *cStr);
String REG_POLY_FUN_HDR(convertBinStringToML, Region rAddr, size_t l, const char *cStr);
String REG_POLY_FUN_HDR(exnNameML, Region rAddr, uintptr_t e);
size_t * REG_POLY_FUN_HDR(explodeStringML, Region rAddr2, String str);  // no region for the cons cells

#endif /* STRING_H */
