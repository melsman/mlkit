/*----------------------------------------------------------------*
 *                        Strings                                 *
 *----------------------------------------------------------------*/

#include <stdio.h>
#include "String.h"
#include "Tagging.h"
#include "List.h"
#include "Region.h"

// allocString: Allocates a string of size in region rAddr. Returns a
//     pointer to the string. Uses alloc to allocate memory for the string,
//     which then exists in contiguous memory because alloc uses
//     malloc when the string cannot fit in a region page. The size is in 
//     bytes, so we have to convert to words, and make alignment. The 
//     content of the string is not initialized. 

static inline String
REG_POLY_FUN_HDR(allocString, Region rAddr, int size) 
{
  String sd;
  int szAlloc;                // size of string in words + tag
  int size0 = size + 1;       // size with space for '\0'
  szAlloc = 1 + ((size0 % 4) ? (size0 / 4)+1 : (size0 / 4));  // 1 is for the tag
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
convertStringToC(String mlStr, unsigned char *buf, int buflen, int exn) 
{
  int i, sz;
  unsigned char *p;

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
REG_POLY_FUN_HDR(convertStringToML, Region rAddr, unsigned char *cStr) 
{
  String res;
  unsigned char *p;
  int i;

  res = REG_POLY_CALL(allocString, rAddr, strlen(cStr));   // sets size also

  for ( p = &(res->data); *cStr != '\0'; )
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
REG_POLY_FUN_HDR(allocStringML, Region rAddr, int sizeML)
{
  int sizeC = convertIntToC(sizeML);
  String strPtr;
  strPtr = REG_POLY_CALL(allocString, rAddr, sizeC);
  return strPtr;
}

int 
chrCharML(int charNrML, int exn) 
{
  int charNrC = convertIntToC(charNrML); 
  if ( charNrC >= 0 && charNrC <= 255 ) 
    {
      return convertIntToML (charNrC); 
    }
  raise_exn(exn);
  return;
}

String
REG_POLY_FUN_HDR(concatStringML, Region rAddr, String str1, String str2) 
{
  String res;
  unsigned char *s, *p;
  int i, sz;
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
  return res;
}

String
REG_POLY_FUN_HDR(implodeCharsML, Region rAddr, int xs) 
{
  String res;
  int length = 0;
  int ys;
  unsigned char *p;

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
REG_POLY_FUN_HDR(implodeStringML, Region rAddr, int xs) 
{
  String res;
  int sz=0;
  int ys;
  unsigned char *p;

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
      int i;
      unsigned char *s;
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
  int min, l1, l2, i;
  unsigned char *p1, *p2;

  l1 = sizeStringDefine(s1);
  l2 = sizeStringDefine(s2);

  if ( l1 < l2 ) min = l1;
  else           min = l2;

  p1 = &(s1->data);
  p2 = &(s2->data);
  
  for ( i = 0; i < min; i++, p1++, p2++ )
    {
      if ( *p1 < *p2 ) return -1;
      if ( *p2 < *p1 ) return  1;
    }
  if ( l1 < l2 ) return -1;
  if ( l2 < l1 ) return  1;
  return 0;
}

int 
lessStringML(String s1, String s2) 
{
  if ( mystrcmp (s1, s2) < 0 )
    {
      return mlTRUE;
    }
  return mlFALSE;
}

int 
lesseqStringML(String s1, String s2) 
{
  if ( mystrcmp (s1, s2) <= 0 )
    {
      return mlTRUE;
    }
  return mlFALSE;
}

int 
greaterStringML(String s1, String s2) 
{
  if ( mystrcmp (s1, s2) > 0 )
    {
      return mlTRUE;
    }
  return mlFALSE;
}

int 
greatereqStringML(String s1, String s2) 
{
  if ( mystrcmp (s1, s2) >= 0 )
    {
      return mlTRUE;
    }
  return mlFALSE;
}

int 
equalStringML(String s1, String s2) 
{
  if ( sizeStringDefine(s1) != sizeStringDefine(s2) )
    {
      return mlFALSE;
    }
  if ( mystrcmp (s1, s2) == 0 )
    {
      return mlTRUE;
    }
  return mlFALSE;
}

// exnNameML: return name of exception; the function 
// is exomorphic by copying
String
REG_POLY_FUN_HDR(exnNameML, Region rAddr, int e) 
{
  String ml_s;

#ifdef TAG_VALUES
  ml_s = (String)(* ( (*((int **)e+1)) + 2));
#else
  ml_s = (String)(* ( (*(int **)e) + 1));
#endif

  return REG_POLY_CALL(convertStringToML, rAddr, &(ml_s->data));
}

/* explodeStringML(rAddr, str): convert a string to a char list. 
 * A list is kept in one region, pointed to by rAddr.  */

int *
REG_POLY_FUN_HDR(explodeStringML, Region rAddr, String str) 
{
  int *res, *consPtr, *pair, *tpair, i, sz;
  unsigned char *p;

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
      second(pair) = (int)consPtr;
      pair = tpair;
    }
  makeNIL(consPtr);
  second(pair) = (int)consPtr;
  return res;
}

// for debugging */
void 
printNum(int n) 
{
  printf("Num: %d\n",convertIntToC(n));
  return;
}
