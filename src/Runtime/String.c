/*----------------------------------------------------------------*
 *                        Strings                                 *
 *----------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "String.h"
#include "Tagging.h"
#include "List.h"
#include "Region.h"
#include "Exception.h"

// allocString: Allocates a string of size in region rAddr. Returns a
//     pointer to the string. Uses alloc to allocate memory for the string,
//     which then exists in contiguous memory because alloc uses
//     malloc when the string cannot fit in a region page. The size is in
//     bytes, so we have to convert to words, and make alignment. The
//     content of the string is not initialized.

static inline String
REG_POLY_FUN_HDR(allocString, Region rAddr, size_t size)
{
  String sd;
  size_t szAlloc;                // size of string in words + tag
  size_t size0 = size + 1;       // size with space for '\0'
  szAlloc = 1 + ((size0 % (sizeof(void *))) ? (size0 / (sizeof(void *)))+1 : (size0 / (sizeof(void *))));  // 1 is for the tag
#ifdef PROFILING
  sd = (String) allocProfiling(rAddr, szAlloc, pPoint);
#else
  sd = (String) alloc(rAddr, szAlloc);
#endif
  sd->size = val_tag_string(size);
  return sd;
}

// convertStringToC: Copy ML string to 'buf' of size 'buflen'
void
convertStringToC(String mlStr, char *buf, size_t buflen, uintptr_t exn)
{
  size_t sz;
  char *p;

  sz = sizeStringDefine(mlStr);
  if ( sz > buflen-1)
    {
      raise_exn(exn);
    }
  for ( p = &(mlStr->data); *p != '\0'; )
    {
      *buf++ = *p++;
    }
  *buf = '\0';
  return;
}

// convertStringToML: The ML string is allocated in the region
// pointen at by rAddr.
String
REG_POLY_FUN_HDR(convertStringToML, Region rAddr, const char *cStr)
{
  String res;
  char *p;

  res = REG_POLY_CALL(allocString, rAddr, strlen(cStr));   // sets size also

  for ( p = &(res->data); *cStr != '\0'; )
    {
      *p++ = *cStr++;
    }
  *p = '\0';
  return res;
}

// convertStringToML: The ML string is allocated in the region pointen
// at by rAddr. This should also work on binary string because we do
// not test on \0 in during the copy. However, you must be sure that
// the legth is correct.
String
REG_POLY_FUN_HDR(convertBinStringToML, Region rAddr, size_t l, const char *cStr)
{
  String res;
  char *p;
  size_t i;

  res = REG_POLY_CALL(allocString, rAddr, l);   // sets size also
  p = &(res->data);
  for (i=0; i<l; i++)
    {
      *p++ = *cStr++;
    }
  *p = '\0';
  return res;
}

/******************************************************************
 * EXTERNAL DECLARATIONS (ML functions, basislib)                 *
 ******************************************************************/

// allocStringML: same as allocString except that it is called from
//     the basis library and size may be tagged.  A
//     string/W8vector/W8array is a scalar value so GC does not require
//     that the string is initialized.
String
REG_POLY_FUN_HDR(allocStringML, Region rAddr, size_t sizeML)
{
  size_t sizeC = convertIntToC(sizeML);
  String strPtr;
  strPtr = REG_POLY_CALL(allocString, rAddr, sizeC);
  return strPtr;
}

String
REG_POLY_FUN_HDR(allocStringC, Region rAddr, size_t sizeC)
{
  String strPtr;
  strPtr = REG_POLY_CALL(allocString, rAddr, sizeC);
  return strPtr;
}

size_t
chrCharML(size_t charNrML, uintptr_t exn)
{
  size_t charNrC = convertIntToC(charNrML);
  if ( charNrC <= 255 )
    {
      return convertIntToML (charNrC);
    }
  raise_exn(exn);
  return 0;        // never reached
}

String
REG_POLY_FUN_HDR(concatStringML, Region rAddr, String str1, String str2)
{
  String res;
  char *s, *p;
  size_t i, sz;
  debug(printf("[enter concatStringML (rAddr=%p,str1=%p,str2=%p)]\n", rAddr,str1,str2);)
  sz = sizeStringDefine(str1) + sizeStringDefine(str2);
  res = REG_POLY_CALL(allocString, rAddr, sz);
  p = &(res->data);
  s = &(str1->data);
  for ( i = 0; i < sizeStringDefine(str1); i++)
    {
      *p++ = *s++;
    }
  s = &(str2->data);
  for ( i = 0; i < sizeStringDefine(str2); i++)
    {
      *p++ = *s++;
    }
  *p = '\0';
/*  printf("\nconcatStringML\n%s\n%s\n  ->  \n%s\n", &(str1->data), &(str2->data), &(res->data));
  printf("length 1: %d, length 2: %d, length 3: %d\n", sizeStringDefine(str1), sizeStringDefine(str2), sizeStringDefine(res)); */
  return res;
}

String
REG_POLY_FUN_HDR(implodeCharsML, Region rAddr, uintptr_t xs)
{
  String res;
  size_t length = 0;
  size_t ys;
  char *p;

  // maybe reset region
  if ( is_inf_and_atbot(rAddr) )
    {
      resetRegion(rAddr);
    }

  // calculate length of string
  for ( ys = xs; isCONS(ys); ys = tl(ys) )
    {
      length++;
    }

  res = REG_POLY_CALL(allocString, rAddr, length);
  p = &(res->data);
  for ( ys = xs; isCONS(ys); ys = tl(ys) )
    {
      *p++ = (unsigned char) convertIntToC (hd(ys));
    }
  *p = '\0';
  return res;
}

// implodeStringML
// Example: ["ABC","DEF","GHI","JKL"]
//   = CONS("ABC",CONS("DEF",CONS("GHI",CONS("JKL",NIL))))
String
REG_POLY_FUN_HDR(implodeStringML, Region rAddr, uintptr_t xs)
{
  String res;
  size_t sz=0;
  size_t ys;
  char *p;

  // calculate string length and allocate
  for ( ys = xs; isCONS(ys); ys = tl(ys) )
    {
      sz += sizeStringDefine(hd(ys));
    }
  res = REG_POLY_CALL(allocString, rAddr, sz);

  p = &(res->data);
  for ( ys = xs; isCONS(ys); ys = tl(ys) )
    {
      String sd;
      size_t i;
      char *s;
      sd = (String)hd(ys);
      s = &(sd->data);
      for ( i = 0; i < sizeStringDefine(sd); i++ )
	{
	  *p++ = *s++;
	}
    }
  *p = '\0';
  return res;
}

void
printStringML(String str)
{
  fputs(&(str->data),stdout);
  fflush(stdout);
  return;
}

static inline int
mystrcmp (String s1, String s2)
{
  size_t min, l1, l2, i;
  unsigned char *p1, *p2;

  if ( s1 == s2 ) return 0;

  l1 = sizeStringDefine(s1);
  l2 = sizeStringDefine(s2);

  if ( l1 < l2 ) min = l1;
  else           min = l2;

  p1 = (unsigned char *) &(s1->data);
  p2 = (unsigned char *) &(s2->data);

  for ( i = 0; i < min; i++, p1++, p2++ )
    {
      if ( *p1 < *p2 ) return -1;
      if ( *p2 < *p1 ) return  1;
    }
  if ( l1 < l2 ) return -1;
  if ( l2 < l1 ) return  1;
  return 0;
}

size_t
lessStringML(String s1, String s2)
{
  if ( mystrcmp (s1, s2) < 0 )
    {
      return mlTRUE;
    }
  return mlFALSE;
}

size_t
lesseqStringML(String s1, String s2)
{
  if ( mystrcmp (s1, s2) <= 0 )
    {
      return mlTRUE;
    }
  return mlFALSE;
}

size_t
greaterStringML(String s1, String s2)
{
  if ( mystrcmp (s1, s2) > 0 )
    {
      return mlTRUE;
    }
  return mlFALSE;
}

size_t
greatereqStringML(String s1, String s2)
{
  if ( mystrcmp (s1, s2) >= 0 )
    {
      return mlTRUE;
    }
  return mlFALSE;
}

size_t
equalStringML(String s1, String s2)
{
  char *p1, *p2;
  size_t sz;
  if (s1 == s2) return mlTRUE;

  sz = sizeStringDefine(s1);
  if ( sz != sizeStringDefine(s2) )
    return mlFALSE;
  for (p1 = &(s1->data), p2 = &(s2->data) ; sz > 0 ; sz-- )
    if ( *p1++ != *p2++ ) return mlFALSE;
  return mlTRUE;
}

// exnNameML: return name of exception; the function
// is exomorphic by copying
String
REG_POLY_FUN_HDR(exnNameML, Region rAddr, uintptr_t e)
{
  String ml_s;

#ifdef TAG_VALUES
  ml_s = (String)(* ( (*((uintptr_t **)e+1)) + 2));
#else
  ml_s = (String)(* ( (*(uintptr_t **)e) + 1));
#endif

  return REG_POLY_CALL(convertStringToML, rAddr, &(ml_s->data));
}

/* explodeStringML(rAddr, str): convert a string to a char list.
 * A list is kept in one region, pointed to by rAddr.  */

uintptr_t *
REG_POLY_FUN_HDR(explodeStringML, Region rAddr, String str)
{
  uintptr_t *res, *consPtr, *pair, *tpair;
  size_t i, sz;
  char *p;

  sz = sizeStringDefine(str);
  if (sz == 0)
    {
      makeNIL(res);
      return res;
    }

   // save first char such that we can return a pointer to it
  p = &(str->data);

#ifdef PROFILING
  allocPairMLProf(rAddr, pair, pPoint);
#else
  allocPairML(rAddr, pair);
#endif

  first(pair) = convertIntToML (*p);
  makeCONS(pair, consPtr);
  res = consPtr;
  for ( i = 1 ; i < sz; i++ )
    {
      #ifdef PROFILING
      allocPairMLProf(rAddr, tpair, pPoint);
      #else
      allocPairML(rAddr, tpair);
      #endif

      first(tpair) = convertIntToML (*p++);
      makeCONS(tpair, consPtr);
      second(pair) = (size_t)consPtr;
      pair = tpair;
    }
  makeNIL(consPtr);
  second(pair) = (size_t)consPtr;
  return res;
}

// for debugging */
void
printNum(ssize_t n)
{
  printf("Num: %d\n",convertIntToC((int)n));
  /*
  asm volatile ( "movq $32, %rdi\n\t"
                 "movq $52, %rsi\n\t"
                 "movq $62, %rdx\n\t"
                 "movq $72, %rcx\n\t"
                 "movq $82, %r8\n\t"
                 "movq $92, %r9\n\t"
                 "movq $102, %r10\n\t"
                 "movq $112, %r11\n\t"
                 "movq $122, %rax\n\t"
                 "movq $132, %rbx\n\t"
                 "movq $142, %r12\n\t"
                 "movq $152, %r13\n\t"
                 "movq $162, %r14\n\t"
                 "movq $172, %r15\n\t"
	       );
  */
  return;
}

// for debugging */
void
printLong(ssize_t n)
{
  printf("Long unsigned: %lu\n", convertIntToC((long int)n));
  printf("Long signed: %ld\n", convertIntToC((long int)n));
  return;
}
