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

static StringDesc *
#ifdef PROFILING
allocStringProfiling(int rAddr, int size, int pPoint)
#else 
allocString(int rAddr, int size) 
#endif
{
  StringDesc * sd;
  int szAlloc;                // size of string in words + size field
  int size0 = size + 1;       // size with space for '\0'
  szAlloc = 1 + ((size0 % 4) ? (size0 / 4)+1 : (size0 / 4));
#ifdef PROFILING
  sd = (StringDesc *) allocProfiling(rAddr, szAlloc, pPoint);
#else
  sd = (StringDesc *) alloc(rAddr, szAlloc);
#endif
  sd->size = val_tag_string(size);
  return sd;
}

// convertStringToC: Copy ML string to 'buf' of size 'buflen'
void 
convertStringToC(StringDesc *mlStr, char *buf, int buflen, int exn) 
{
  int i, sz;
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
StringDesc *
#ifdef PROFILING
convertStringToMLProfiling(int rAddr, char *cStr, int pPoint) 
#else
convertStringToML(int rAddr, char *cStr) 
#endif
{
  StringDesc *res;
  char *p;
  int i;

  #ifdef PROFILING
  res = allocStringProfiling(rAddr, strlen(cStr), pPoint);   // sets size also
  #else
  res = allocString(rAddr, strlen(cStr));                    // sets size also
  #endif

  for ( p = &(res->data); *cStr != '\0'; )
    {
      *p++ = *cStr++;
    }
  *p = '\0';
  return res;
}

// printStringList: Print the strings in a list of ML-strings
/*
void 
printStringList(int xs) 
{
  for ( ; isCONS(xs); xs = tl(xs) )
    {
      printStringML((StringDesc *) hd(xs));
    }
}
*/

// makeChar: convert a char to a string
/*
StringDesc *
#ifdef PROFILING
makeCharProfiling(int rAddr, char ch, int pPoint) 
#else
makeChar(int rAddr, char ch) 
#endif
{
  StringDesc *res;
#ifdef PROFILING
  res = allocStringProfiling(rAddr, 1, pPoint);
#else
  res = allocString(rAddr, 1);
#endif
  res->data = ch;                  // ch untagged
  *(&(res->data)+1) = '\0';
  return res;
}
*/

/******************************************************************
 * EXTERNAL DECLARATIONS (ML functions, basislib)                 *
 ******************************************************************/

// allocStringML: same as allocString except that it is called from
//     the basis library and size may be tagged.  A
//     string/W8vector/W8array is a scalar value so GC does not require
//     that the string is initialized.
StringDesc *
#ifdef PROFILING
allocStringProfilingML(int rAddr, int sizeML, int pPoint) 
#else
allocStringML(int rAddr, int sizeML) 
#endif
{
  int sizeC = convertIntToC(sizeML);
  StringDesc *strPtr;
#ifdef PROFILING
  strPtr = allocStringProfiling(rAddr,sizeC,pPoint);
#else
  strPtr = allocString(rAddr,sizeC);
#endif
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

int 
sizeStringML(StringDesc *s) 
{
  return convertIntToML(sizeStringDefine(s));
}

StringDesc *
#ifdef PROFILING
concatStringProfilingML(int rAddr, StringDesc *str1, StringDesc *str2, int pPoint) 
#else
concatStringML(int rAddr, StringDesc *str1, StringDesc *str2) 
#endif
{
  StringDesc *res;
  char *s, *p;
  int i, sz;
  sz = sizeStringDefine(str1) + sizeStringDefine(str2);
#ifdef PROFILING
  res = allocStringProfiling(rAddr, sz, pPoint);
#else
  res = allocString(rAddr, sz);
#endif
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

StringDesc *
#ifdef PROFILING
implodeCharsProfilingML(int rAddr, int xs, int pPoint) 
#else
implodeCharsML(int rAddr, int xs) 
#endif
{
  StringDesc *res;
  int length = 0;
  int ys;
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

#ifdef PROFILING
  res = allocStringProfiling (rAddr, length, pPoint);
#else
  res = allocString (rAddr, length);
#endif
  p = &(res->data);
  for ( ys = xs; isCONS(ys); ys = tl(ys) ) 
    {
      *p++ = (char) convertIntToC (hd(ys));
    }
  *p = '\0';
  return res;
}

// implodeStringML
// Example: ["ABC","DEF","GHI","JKL"]                            
//   = CONS("ABC",CONS("DEF",CONS("GHI",CONS("JKL",NIL))))
StringDesc *
#ifdef PROFILING
implodeStringProfilingML(int rAddr, int xs, int pPoint) 
#else
implodeStringML(int rAddr, int xs) 
#endif
{
  StringDesc *res;
  int sz=0;
  int ys;
  char* p;

  // calculate string length and allocate
  for ( ys = xs; isCONS(ys); ys = tl(ys) )
    {
      sz += sizeStringDefine(hd(ys));
    }

  #ifdef PROFILING
  res = allocStringProfiling(rAddr, sz, pPoint);
  #else
  res = allocString(rAddr, sz);
  #endif

  p = &(res->data);
  for ( ys = xs; isCONS(ys); ys = tl(ys) )
    {
      StringDesc* sd;
      int i;
      char *s;
      sd = (StringDesc*)hd(ys);
      s = &(sd->data);
      for ( i = 0; i < sizeStringDefine(sd); i++ )
	{
	  *p++ = *s++;
	}
    }
  *p = '\0';
  return res;
}

// subStringML: returns the i'th character of the string.
int 
subStringML(StringDesc *str,int iML) 
{
  int iC;
  char ch;
  iC = convertIntToC(iML);
  ch = *(&(str->data) + iC);  
  return convertIntToML(ch);
}

// updateStringML: updates the i'th character of the string.
void 
updateStringML(StringDesc *str, int iML, int cML) 
{
  int iC;
  char ch;
  iC = convertIntToC(iML);
  *(&(str->data) + iC) = (char)(convertIntToC(cML));
  return;
}

void 
printStringML(StringDesc *str) 
{
  fputs(&(str->data),stdout);
  fflush(stdout);
  return;
}

static int
mystrcmp (StringDesc *s1, StringDesc *s2) 
{
  int min, l1, l2;
  char *p1, *p2;
  l1 = sizeStringDefine(s1);
  l2 = sizeStringDefine(s2);
  if ( l1 < l2 ) min = l1;
  else min = l2;
  p1 = &(s1->data);
  p2 = &(s2->data);
  
  return memcmp(p1,p2,min+1);
}

int 
lessStringML(StringDesc *s1, StringDesc *s2) 
{
  if ( mystrcmp (s1, s2) < 0 )
    {
      return mlTRUE;
    }
  return mlFALSE;
}

int 
lesseqStringML(StringDesc *s1, StringDesc *s2) 
{
  if ( mystrcmp (s1, s2) <= 0 )
    {
      return mlTRUE;
    }
  return mlFALSE;
}

int 
greaterStringML(StringDesc *s1, StringDesc *s2) 
{
  if ( mystrcmp (s1, s2) > 0 )
    {
      return mlTRUE;
    }
  return mlFALSE;
}

int 
greatereqStringML(StringDesc *s1, StringDesc *s2) 
{
  if ( mystrcmp (s1, s2) >= 0 )
    {
      return mlTRUE;
    }
  return mlFALSE;
}

int 
equalStringML(StringDesc *s1, StringDesc *s2) 
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
StringDesc *
#ifdef PROFILING
exnNameProfilingML(int rAddr, int e, int pPoint) 
#else
exnNameML(int rAddr, int e) 
#endif
{
  StringDesc *ml_s;

#ifdef TAG_VALUES
  ml_s = (StringDesc*)(* ( (*((int **)e+1)) + 2));
#else
  ml_s = (StringDesc*)(* ( (*(int **)e) + 1));
#endif

#ifdef PROFILING
  return convertStringToMLProfiling(rAddr, &(ml_s->data), pPoint);
#else
  return convertStringToML(rAddr, &(ml_s->data));
#endif
}

/* explodeStringML(rAddr, str): convert a string to a char list. 
 * A list is kept in one region, pointed to by rAddr.  */

int *
#ifdef PROFILING
explodeStringProfiling(int rAddr, StringDesc *str, int pPoint) 
#else
explodeStringML(int rAddr, StringDesc *str) 
#endif
{
  int *res, *consPtr, *pair, *tpair, i, sz;
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
  allocRecordMLProf(rAddr, 2, pair, pPoint);
#else
  allocRecordML(rAddr, 2, pair);
#endif

  first(pair) = convertIntToML (*p);
  makeCONS(pair, consPtr);
  res = consPtr;
  for ( i = 1 ; i < sz; i++ )
    {

      #ifdef PROFILING
      allocRecordMLProf(rAddr, 2, tpair, pPoint); 
      #else
      allocRecordML(rAddr, 2, tpair); 
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
