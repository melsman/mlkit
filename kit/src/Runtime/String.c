/*----------------------------------------------------------------*
 *                        Strings                                 *
 *----------------------------------------------------------------*/
#include <stdio.h>
#include "String.h"
#include "Tagging.h"
#include "List.h"
#include "Region.h"
#include "Exception.h"

#define  minDefine(A,B) ((A<B)?A:B)

/*----------------------------------------------------------------*
 * Global declarations                                            *
 *----------------------------------------------------------------*/
char cString[C_STRING_LENGTH];  /* We allocate a global string for converting filenames in IO operations. */


/*------------------------------------------------------------------*
 *                 Runtime system for strings.                      *
 *                                                                  *
 * allocString: Allocate a string of fixed length in a region.      *
 * chrChar: Return a char representing the value given.             *
 *   since chars are represented as ints, this is the identity      *
 *   function, except it may raise Chr if the int is not in [0;255] *
 * sizeString: Return the size of a string represented as 2i+1.     *
 * convertString: Converts an ML-string to a C-string.              *
 * convertFragment: Converts a stringfragment to a C-String.        *
 * equalString: Returns TRUE (mlTRUE) when two strings are equal.   *
 * lessString:                                                      *
 * lesseqString:                                                    *
 * greaterString:                                                   *
 * greatereqString:                                                 *
 * copyString: Copy string str1 into str2.                          *
 * concatString: Concats str1 and str2.                             *
 * printString: Prints all fragments in a ML-string.                *
 * printStringList: Print the strings in a ML-string list.          *
 * reverseStringFrag: Reverses the fragments in a ML-string.        *
 * implodeChars: Implodes a list of chars.                          *
 * explodeString: Explodes a ML-string to a list of ML-chars.       *
 * exnName: Returns a string representing the identifier of an exn. *
 *------------------------------------------------------------------*/

/*-----------------------------------------------------------*
 * allocString: Allocates a string of size in region rAddr.  *
 *              Returns a pointer to the string.             *
 *              It will fill out the last regionpage, before *
 *              a new page is requested.                     *
 *              size is in bytes, so we have to convert to   *
 *              words, and make alignment.                   *
 *-----------------------------------------------------------*/
StringDesc *allocString(int rAddr, int size) {
  Ro *rp;
  StringDesc * returnPtr;
  StringFragment * fragPtr;
  int free_in_page;       /* Number of words free in the last regionpage. */
  int sizeWords;  /* size of the string in words. */

  rp = (Ro *) clearStatusBits(rAddr);
  free_in_page = freeInRegion(rp); /* free is in words. */
  sizeWords = ((size % 4) ? (size / 4)+1 : (size / 4));
  if (free_in_page*4 > sizeof(StringDesc)) { /* The last regionpage can hold one fragment. */
    returnPtr = (StringDesc *) alloc(rAddr, minDefine(free_in_page, (sizeof(StringDesc)/4)+sizeWords));
    returnPtr->sf.fragmentSize = minDefine(free_in_page*4-sizeof(StringDesc),size);
  }
  else { /* The last regionpage can't hold one fragment. */
    returnPtr = (StringDesc *) alloc(rAddr, minDefine(ALLOCATABLE_WORDS_IN_REGION_PAGE, (sizeof(StringDesc)/4)+sizeWords));
    returnPtr->sf.fragmentSize = minDefine(ALLOCATABLE_WORDS_IN_REGION_PAGE*4-sizeof(StringDesc),size);
  }

  returnPtr->stringSize = (size << 3)+valueTagString;   /* We do not tag the size, only the string. */

  returnPtr->sf.n = NULL;
  fragPtr = &(returnPtr->sf);
  size -= returnPtr->sf.fragmentSize;

  while (size > 0) {
    sizeWords = ((size % 4) ? (size / 4)+1 : (size / 4));
    fragPtr->n = (StringFragment *) alloc(rAddr, minDefine(ALLOCATABLE_WORDS_IN_REGION_PAGE, 
							   (sizeof(StringFragment)/4)+sizeWords));
    fragPtr = fragPtr->n;
    fragPtr->fragmentSize = minDefine(ALLOCATABLE_WORDS_IN_REGION_PAGE*4-sizeof(StringFragment), size);
    fragPtr->n = NULL;
    size -= fragPtr->fragmentSize;
  }

  return returnPtr; 
}



/*--------------------------------------------------------------------*
 * chrChar: Return a char representing                                *
 *            the value given. If the value is not in the interval    *
 *            0..255 the exeption Chr is raised.                      *
 *--------------------------------------------------------------------*/

int chrChar (int charNr, int exn) {

  charNr = convertIntToC(charNr); /* We have to untag the charNr. */
  if (charNr >= 0 && charNr <= 255) {
    /*08/07/1997 23:22. tho.  TODO:  untagging, checking, & then 
      tagging immediately afterwards is maybe a bit silly:*/
    return convertIntToML (charNr); 
  }
  else {
    raise_exn(exn);
    return;
  }
}

/*--------------------------------------------------------------------*
 * sizeString: Return the size of a string represented as ML int.     *
 *--------------------------------------------------------------------*/
int sizeString(StringDesc *str) {
  int i;
  i = convertIntToML(sizeStringDefine(str)); /* We have to tag the size. */
  return i;
}

/*-------------------------------------------------------------------*
 * convertString:                                                    *
 *   To implement file output and input, we have a function that     *
 *   converts a StringDesc to a C-string. The C-string is            *
 *   statically defined, so there is only one C-string at the time,  *
 *   and it has a maximum defined length (C_STRING_LENGTH).          *
 *   It is used when a file has to be opened for read or write. The  *
 *   name of the file has to be converted from a StringDesc to a     *
 *   C-string.                                                       *
 *-------------------------------------------------------------------*/
void convertString(StringDesc *str) {
  int i;
  StringFragment *fragPtr;
  char *ch=&cString[0], *fragCh;

  if (sizeStringDefine(str) > C_STRING_LENGTH-1)
    printf("convertString: String too large.\n");
  else {
    for (fragPtr=&(str->sf);fragPtr;fragPtr=fragPtr->n) {
      fragCh = (char *) (fragPtr+1);
      for (i=0;i<(fragPtr->fragmentSize);i++)
        *ch++=*fragCh++;
      *ch = '\0';
    }
  }
}

/*---------------------------------------------------------------------*
 * convertFragment: Converts a stringfragment to the cString constant. *
 *---------------------------------------------------------------------*/
void convertFragment(StringFragment *fragPtr) {
  int i;
  char *ch=&cString[0], *fragCh;

  if (fragPtr->fragmentSize > C_STRING_LENGTH-1)
    printf("convertFragment: Fragment too large.\n");
  else {
    fragCh = (char *) (fragPtr+1);
    for (i=0;i<(fragPtr->fragmentSize);i++)
      *ch++=*fragCh++;
    *ch = '\0';
  }
}



/*cmpString (str1, str2, less, equal, greater) = less, equal or 
 greater, according to whether the string str1 is less than, equal
 to or greater than the string str2.  
 cmpString is only used by lessString, lesseqString, greaterString
 & greatereqString.  27/06/1997 16:11. tho.*/

int 
cmpString (StringDesc *str1, StringDesc *str2, int less, int equal, int greater) {

  StringFragment *fragPtr1, *fragPtr2;
  int fragSize1, fragSize2;
  int numToExamine_in_all, numToExamine_in_this_frag, i;
  int size1, size2;
  char *ch1, *ch2;
  
  fragPtr1  = &(str1->sf);
  fragSize1 = fragPtr1->fragmentSize;
  ch1       = (char *)(fragPtr1+1);
  fragPtr2  = &(str2->sf);
  fragSize2 = fragPtr2->fragmentSize;
  ch2       = (char *)(fragPtr2+1);
  
  size1 = sizeStringDefine (str1);
  size2 = sizeStringDefine (str2);
  numToExamine_in_all = minDefine (size1, size2);
  while (0 != numToExamine_in_all) {
    numToExamine_in_this_frag = minDefine(fragSize1, fragSize2);
    numToExamine_in_all -= numToExamine_in_this_frag;
    for (i=0; i<numToExamine_in_this_frag; i++) {
      if (*ch1++ != *ch2++) goto diff_found;}
    if (numToExamine_in_this_frag == fragSize1) {
      fragPtr1 = fragPtr1->n;
      if (fragPtr1 != NULL) { /*26/03/1997, Niels*/
	fragSize1 = fragPtr1->fragmentSize;
	ch1 = (char *)(fragPtr1+1); }}
    else
      fragSize1 -= numToExamine_in_this_frag;
    if (numToExamine_in_this_frag == fragSize2) {
      fragPtr2 = fragPtr2->n;
      if (fragPtr2 != NULL) { /*26/03/1997, Niels*/
	fragSize2 = fragPtr2->fragmentSize;
	ch2 = (char *)(fragPtr2+1);}}
    else
      fragSize2 -= numToExamine_in_this_frag;}

  /*no diff found, the string lengths must decide it:*/
  if (size1 < size2)      { return less; }
  else if (size1 > size2) { return greater; }
  else                    { return equal; }

diff_found: /*ch1-1 and ch2-1 point to chars in str1 & str2 that are not the same*/
  return (*(ch1-1) < *(ch2-1) ? less : greater);
}

int
lessString (StringDesc *str1, StringDesc *str2) {
  return cmpString (str1, str2, mlTRUE, mlFALSE, mlFALSE); }

int
lesseqString (StringDesc *str1, StringDesc *str2) {
  return cmpString (str1, str2, mlTRUE, mlTRUE, mlFALSE); }

int
greaterString (StringDesc *str1, StringDesc *str2) {
  return cmpString (str1, str2, mlFALSE, mlFALSE, mlTRUE); }

int
greatereqString (StringDesc *str1, StringDesc *str2) {
  return cmpString (str1, str2, mlFALSE, mlTRUE, mlTRUE); }

int
equalString (StringDesc *str1, StringDesc *str2) {
  return cmpString (str1, str2, mlFALSE, mlTRUE, mlFALSE); }


/*---------------------------------------------------------------------*
 *  copyString: Copy string str1 into str2. It requires that they are  *
 *              allocated, and especially str2 has to be long enough.  *
 *              It is not trivial to do the copy because the strings   *
 *              can be differently fragmented.                         *
 *              It is important, that it is only a fragment of str2    *
 *              that is given as argument.                             *
 *              Because the strings are already allocated, we don't    *
 *              have to set the size of the fragments.                 *
 *              It returns a pointer to the next unused character      *
 *              allocated in str2, and the pointer str2 is adjusted to *
 *              the fragment where the next unused character reside.   *
 *---------------------------------------------------------------------*/
char *copyString(StringDesc *str1, StringFragment **str2, char *ch2) {

  StringFragment *fragPtr1, *fragPtr2;
  int fragSize1, fragSize2, totalSize, numToCopy, i;
  char *ch1;

  totalSize = sizeStringDefine(str1);
  if (totalSize > 0) {
    fragPtr1  = &(str1->sf);
    fragSize1 = fragPtr1->fragmentSize;
    fragPtr2  = *str2;
    fragSize2 = (fragPtr2->fragmentSize)-((int)ch2-(int)(fragPtr2+1));
    ch1 = (char *) (str1+1);

    while (totalSize > 0) {
      numToCopy = minDefine(fragSize1, fragSize2);
      for (i=0;i<numToCopy;i++) {
	*ch2++ = *ch1++;
      }
      totalSize -= numToCopy;
      if (numToCopy == fragSize1) {
	fragPtr1 = fragPtr1->n;
	if (fragPtr1 != NULL) {
	  fragSize1 = fragPtr1->fragmentSize;
	  ch1 = (char *) (fragPtr1+1);
	}
      } else
	fragSize1 -= numToCopy;
      if (numToCopy == fragSize2) {
	fragPtr2 = fragPtr2->n;
	if (fragPtr2 != NULL) {
	  fragSize2 = fragPtr2->fragmentSize;
	  ch2 = (char *) (fragPtr2+1);
	}
      } else
	fragSize2 -= numToCopy;
    }
    (*str2) = fragPtr2;
  }
  return ch2;
}

/*------------------------------------------------------------------*
 * concatString: Concats str1 and str2, and returns a new string in *
 *               region rAddr.                                      *
 *------------------------------------------------------------------*/
StringDesc *concatString(int rAddr, StringDesc *str1, StringDesc *str2) {
  StringDesc *res;
  StringFragment *fragPtr;
  res = allocString(rAddr, sizeStringDefine(str1)+sizeStringDefine(str2));
  fragPtr = &(res->sf);
  copyString(str2, &fragPtr, copyString(str1, &fragPtr, (char *) (res+1)));
  return res;
}

/*----------------------------------------------------------------*
 * printString: Print all fragments in a string one at a time     *
 * and do a flush.                                                *
 *----------------------------------------------------------------*/
void printString(StringDesc *str) {
  StringFragment *fragPtr;
  for (fragPtr=&(str->sf);fragPtr;fragPtr=fragPtr->n) {
    convertFragment(fragPtr);
    printf("%s",cString);
  }
  fflush(stdout);
}

/*-------------------------------------------------------------*
 * subString: returns the i'th character of the string.         *
 *-------------------------------------------------------------*/

int subString(StringDesc *str,int i) {
  
  unsigned char ch;
  StringFragment *fragPtr;

  fragPtr=&(str->sf);

  for (fragPtr=&(str->sf);fragPtr->fragmentSize <= i; fragPtr = fragPtr->n)
    i = i - fragPtr->fragmentSize; 

  ch = *((unsigned char *)(fragPtr)+8+i); /* 8 is the size of two words containing the fragment size 
					   * and next pointer (mads) */
  return (int)ch;
}


/*-------------------------------------------------------------*
 * updateString: updates the i'th character of the string.         *
 *-------------------------------------------------------------*/

void updateString(StringDesc *str, int i, int c) {
  
  unsigned char ch;
  StringFragment *fragPtr;

  ch = (unsigned char)c;

  /*   printf("enter updateString: %d with %c\n",i,c); */

  fragPtr=&(str->sf);

  /*
  printf("fragPtr: %d\n",(int)fragPtr);
  printf("size: %d\n",(int)(fragPtr->fragmentSize));
  printf("n: %d\n",(int)(fragPtr->n));
  */

    for (fragPtr=&(str->sf);fragPtr->fragmentSize <= i; fragPtr = fragPtr->n){   
      i = i - fragPtr->fragmentSize; 
                                     
    /* printf("updateString loop: %d\n",i)*/
         
    }  

    *((unsigned char *)(fragPtr)+8+i) = ch; /* 8 is the size of two words containing the fragment size 
					       and next pointer (mads) */

  /*  printf("updated string with %d\n",(int)ch); */
}

/*-------------------------------------------------------------*
 * printStringList: Print the strings in a list of ML-strings. *
 *-------------------------------------------------------------*/
void printStringList(int xs) {
  int ys;

  for (ys=xs;isCONS(ys);ys=tl(ys))
    printString((StringDesc *) hd(ys));
}


/*--------------------------------------------------------------------*
 * reverseStringFrag: Reverses the fragments in a string, but NOT the *
 *                    elements in each fragment.                      *
 *                    Returns a pointer to the first fragment in the  *
 *                    reversed list of fragments.                     *
 * Is actually not used in this implementation - yet, and therefore   *
 * not tested.                                                        *
 *--------------------------------------------------------------------*/
/*
StringFragment *reverseStringFrag(StringFragment *fragPtr) {

  StringFragment *fragPtr1, *fragPtr2;

  fragPtr1 = fragPtr->n;
  fragPtr->n = NULL;
  while (fragPtr1) {
    fragPtr2 = fragPtr1->n;
    fragPtr1->n = fragPtr;
    fragPtr = fragPtr1;
    fragPtr1 = fragPtr2;
  }
  return fragPtr;
}
*/


/*----------------------------------------------------------------------*
 *implodeChars (int rAdrr, int xs) = convert a char list
 *(pointed to by xs) to a string which is put in the region
 *pointed to by rAdrr.  The list representation is shown earlier
 *in this module.
 *----------------------------------------------------------------------
 */

StringDesc *implodeChars (int rAddr, int xs) {

  StringDesc *res;
  StringFragment *fragPtr;
  int length;
  int i;
  int ys;
  char *ch;

  /* maybe reset the region for the result */
  if (is_inf_and_atbot(rAddr)) resetRegion(rAddr);

  /*calculate the length of the resultstring:*/
  length = 0;
  for (ys=xs; isCONS(ys); ys=tl(ys)) length++;
  res = allocString (rAddr, length);

  /*copy length chars from xs to the string:*/
  ys = xs;
  fragPtr = &(res->sf);
  while (length > 0) {
    /*copy one fragment at a time:*/
    ch = (char *) (fragPtr+1);
    i = 0; 
    while (i < (fragPtr->fragmentSize) && length > 0) {
      i++;
      length--;
      *ch++ = (char) convertIntToC (hd (ys));
      ys = tl (ys); }
    fragPtr = fragPtr->n; }

  return res;
}


/*----------------------------------------------------------------------*
 * implodeString: Takes a list of strings and gives a new string in     *
 *                region rAddr. The list representation is shown        *
 *                erlier in this module.                                *
 * Example of xs: ["ABC","DEF","GHI","JKL"]                             *
 *                = CONS("ABC",CONS("DEF",CONS("GHI",CONS("JKL",NIL)))) *
 *----------------------------------------------------------------------*/
StringDesc *implodeString(int rAddr, int xs) {

  StringDesc *res;
  int totalSize=0;
  int ys;
  char *ch;
  StringFragment *fragPtr;

  /* We have to calculate the length of the resultstring. */
  for (ys=xs;isCONS(ys);ys=tl(ys))
    totalSize += sizeStringDefine(hd(ys));
  res = allocString(rAddr, totalSize);

  /* Run through the list and copy the strings. */
  ch = (char *) (res+1);
  fragPtr = &(res->sf);
  for (ys=xs; isCONS(ys); ys=tl(ys))
    ch = copyString((StringDesc *) hd(ys), &fragPtr, ch);

  return res;
}


/*explodeString ([rAdrr1, ]rAdrr2, str) = convert a string to a char list.  
  If UNBOX_LISTS, a lists is kept in one region, pointed to by rAdrr2.
  If not UNBOX_LISTS, a list is spread out on 2 regions: one for the pair 
  cells (rAddr2) & one for the CONS cells (rAddr1)*/

#if UNBOX_LISTS
int *explodeString(int rAddr2, /*int rAddr3,*/ StringDesc *str) {
                              /*rAddr3 was for the singleton strings*/
  int *res, *consPtr, *pair, *tpair, i;
  char *ch;
  StringDesc *charPtr;
  StringFragment *fragPtr;

  if (sizeStringDefine(str) == 0)
    makeNIL(res)
  else {
    /* First char is special, because we have to return a ptr to it. */
    fragPtr = &(str->sf);
    allocRecordML(rAddr2, 2, pair);
    first(pair) = convertIntToML (*(char *)(fragPtr+1)); 
    makeCONS(pair, consPtr);
    res = consPtr;
    i = 1;
    for (;fragPtr;fragPtr=fragPtr->n) {
      ch = ((char *)(fragPtr+1))+i;
      for (;i<fragPtr->fragmentSize;i++) {
	allocRecordML(rAddr2, 2, tpair); 
	first(tpair) = convertIntToML (*ch++);
	makeCONS(tpair, consPtr);
	second(pair) = (int) consPtr;
	pair = tpair;
      }
      i=0; /* Start with first character in the following fragments. */
    }
    makeNIL(consPtr)
    second(pair) = (int) consPtr;
  }
  return res;
}

#else /* don't unbox lists */

int *explodeString(int rAddr1, int rAddr2, /*int rAddr3,*/ StringDesc *str) {
                   /*for cons*/ /*pairs*/  /*for chars---no longer needed*/
  int *res, *consPtr, *pair, *tpair, i;
  char *ch;
  StringDesc *charPtr;
  StringFragment *fragPtr;

  if (sizeStringDefine(str) == 0)
    makeNIL(rAddr1, res)
  else {
    /* First char is special, because we have to return a ptr to it. */
    fragPtr = &(str->sf);
    allocRecordML(rAddr2, 2, pair);
    /*first(x) betyder `*x' */
    first(pair) = convertIntToML (*(char *)(fragPtr+1));
    makeCONS(rAddr1, pair, consPtr);
    res = consPtr;
    i = 1;
    for (;fragPtr;fragPtr=fragPtr->n) {
      ch = ((char *)(fragPtr+1))+i;
      for (;i<fragPtr->fragmentSize;i++) {
	allocRecordML(rAddr2, 2, tpair); 
	first(tpair) = convertIntToML (*ch++);
	makeCONS(rAddr1, tpair, consPtr);
	second(pair) = (int) consPtr;
	pair = tpair;
      }
      i=0; /* Start with first character in the following fragments. */
    }
    makeNIL(rAddr1, consPtr)
    second(pair) = (int) consPtr;
  }
  return res;
}
#endif /*UNBOX_LISTS*/


StringDesc *exnName(int rAddr, int e) {
  StringDesc *ml_s;
  StringDesc *ml_empty;
  StringDesc *res;
  char *empty = "";

  ml_s = (StringDesc*)(* ( (*(int **)e) + 1));
  ml_empty = convertStringToML(rAddr, empty);  /* the empty ML string */
  res = concatString(rAddr, ml_empty, ml_s);   /* copy the string */

  return res;
}

StringDesc *makeChar(int rAddr, char ch) {
  StringDesc *res;
  res = allocString(rAddr, 1);
  *(char *)(res+1) = ch;
  return res;
}


/*-------------------------------------------------------------------*
 *                Profiling functions.                               *
 *-------------------------------------------------------------------*/
#ifdef PROFILING

/***************************************************************************
 *     Changed runtime operations for making profiling possible.           *
 *                                                                         *
 * allocStringProfiling(rAddr, size, pPoint)                               *
 * implodeCharsProfiling(rAddr, xs, pPoint)                                *
 * explodeStringProfiling(rAddr1, rAddr2, str, pPoint)                     *
 * concatStringProfiling(rAddr, str1, str2, pPoint)                        *
 ***************************************************************************/

/*-----------------------------------------------------------*
 * allocString: Allocates a string of size in region rAddr.  *
 *              Returns a pointer to the string.             *
 *              It will fill out the last regionpage, before *
 *              a new page is requested.                     *
 *              size is in bytes, so we have to convert to   *
 *              words, and make allignment.                  *
 *              It works ok with size = 0.                   *
 *-----------------------------------------------------------*/
StringDesc *allocStringProfiling(int rAddr, int size, int pPoint) {
  Ro *rp;
  StringDesc * returnPtr;
  StringFragment * fragPtr;
  int free_in_page;       /* Number of words free in the last regionpage. */
  int sizeWords;  /* size of the string in words. */

  rp = (Ro *) clearStatusBits(rAddr);
  free_in_page = freeInRegion(rp); /* free is in words. */
  sizeWords = ((size % 4) ? (size / 4)+1 : (size / 4));
  if (free_in_page*4 > sizeof(StringDesc)+sizeof(ObjectDesc)) { /* The last regionpage can hold one fragment. */
    returnPtr = (StringDesc *) allocProfiling(rAddr, minDefine(free_in_page-sizeObjectDesc,
							       (sizeof(StringDesc)/4)+sizeWords),
					      pPoint);
    returnPtr->sf.fragmentSize = minDefine(free_in_page*4-sizeof(StringDesc)-sizeof(ObjectDesc),size);
  }
  else { /* The last regionpage can't hold one fragment. */
    returnPtr = (StringDesc *) allocProfiling(rAddr,
					      minDefine(ALLOCATABLE_WORDS_IN_REGION_PAGE-sizeObjectDesc,
							(sizeof(StringDesc)/4)+sizeWords),
					      pPoint);
    returnPtr->sf.fragmentSize = minDefine(ALLOCATABLE_WORDS_IN_REGION_PAGE*4-sizeof(StringDesc)-sizeof(ObjectDesc),size);
  }

  returnPtr->stringSize = (size << 3)+valueTagString;   /* We do not tag the size, only the string. */

  returnPtr->sf.n = NULL;
  fragPtr = &(returnPtr->sf);
  size -= returnPtr->sf.fragmentSize;

  while (size > 0) {
    sizeWords = ((size % 4) ? (size / 4)+1 : (size / 4));
    fragPtr->n = (StringFragment *) allocProfiling(rAddr,
						   minDefine(ALLOCATABLE_WORDS_IN_REGION_PAGE-sizeObjectDesc, 
							     (sizeof(StringFragment)/4)+sizeWords),
						   pPoint);
    fragPtr = fragPtr->n;
    fragPtr->fragmentSize = minDefine(ALLOCATABLE_WORDS_IN_REGION_PAGE*4-sizeof(StringFragment)-sizeof(ObjectDesc),size);
    fragPtr->n = NULL;
    size -= fragPtr->fragmentSize;
  }
  return returnPtr; 
}


StringDesc *makeCharProfiling(int rAddr, char ch, int pPoint) {
  StringDesc *res;
  res = allocStringProfiling(rAddr, 1, pPoint);
  *(char *)(res+1) = ch;
  return res;
}


StringDesc *implodeCharsProfiling (int rAddr, int xs, int pPoint) {

  /* for comments, see implodeChars, which is almost 
   * exactly like implodeCharsProfiling*/
  StringDesc *res;
  StringFragment *fragPtr;
  int length;
  int i;
  int ys;
  char *ch;

  /* maybe reset the region for the result */
  if (is_inf_and_atbot(rAddr)) resetRegion(rAddr);

  length = 0;
  for (ys=xs; isCONS(ys); ys=tl(ys)) length++;
  res = allocStringProfiling (rAddr, length, pPoint);

  ys = xs;
  fragPtr = &(res->sf);
  while (length > 0) {
    ch = (char *) (fragPtr+1);
    i = 0;
    while (i < (fragPtr->fragmentSize) && length > 0) {
      i++;
      length--;
      *ch++ = (char) convertIntToC (hd (ys));
      ys = tl (ys); 
    }
    fragPtr = fragPtr->n; 
  }
  return res;
}


/*----------------------------------------------------------------------*
 * implodeString: Takes a list of strings and gives a new string in     *
 *                region rAddr. The list representation is shown        *
 *                erlier in this module.                                *
 * Example of xs: ["ABC","DEF","GHI","JKL"]                             *
 *                = CONS("ABC",CONS("DEF",CONS("GHI",CONS("JKL",NIL)))) *
 *----------------------------------------------------------------------*/
StringDesc *implodeStringProfiling(int rAddr, int xs, int pPoint) {

  StringDesc *res;
  int totalSize=0;
  int ys;
  char *ch;
  StringFragment *fragPtr;

  /* We have to calculate the length of the result string. */
  for (ys=xs;isCONS(ys);ys=tl(ys))
    totalSize += sizeStringDefine(hd(ys));
  res = allocStringProfiling(rAddr, totalSize, pPoint);

  /* Run through the list and copy strings. */
  ch = (char *) (res+1);
  fragPtr = &(res->sf);
  for (ys=xs; isCONS(ys); ys=tl(ys))
    ch = copyString((StringDesc *) hd(ys), &fragPtr, ch);

  return res;
}


#if UNBOX_LISTS
int *explodeStringProfiling(int rAddr2, /*int rAddr3,*/ StringDesc *str, int pPoint) {

  /*for comments see explodeString above*/
  int *res, *consPtr, *pair, *tpair, i;
  char *ch;
  StringDesc *charPtr;
  StringFragment *fragPtr;

  if (sizeStringDefine(str) == 0)
    makeNIL(res)
  else {
    /* First char is special, because we have to return a ptr to it. */
    fragPtr = &(str->sf);
    allocRecordMLProf(rAddr2, 2, pair, pPoint);
    first(pair) = convertIntToML (*(char *)(fragPtr+1)); 
    makeCONS(pair, consPtr);
    res = consPtr;
    i = 1;
    for (;fragPtr;fragPtr=fragPtr->n) {
      ch = ((char *)(fragPtr+1))+i;
      for (;i<fragPtr->fragmentSize;i++) {
	allocRecordMLProf(rAddr2, 2, tpair, pPoint);
	first(tpair) = convertIntToML (*ch++);
	makeCONS(tpair, consPtr);
	second(pair) = (int) consPtr;
	pair = tpair;
      }
      i=0; /* Start with first character in the following fragments. */
    }
    makeNIL(consPtr)
    second(pair) = (int) consPtr;
  }

  return res;
}

#else /* don't unbox lists */

int *explodeStringProfiling(int rAddr1, int rAddr2, /*int rAddr3,*/ StringDesc *str, int pPoint) {

  /*for comments see explodeString above*/
  int *res, *consPtr, *pair, *tpair, i;
  char *ch;
  StringDesc *charPtr;
  StringFragment *fragPtr;

  if (sizeStringDefine(str) == 0)
    makeNILProf(rAddr1, res, pPoint)
  else {
    /* First char is special, because we have to return a ptr to it. */
    fragPtr = &(str->sf);
    allocRecordMLProf(rAddr2, 2, pair, pPoint);
    first(pair) = convertIntToML (*(char *)(fragPtr+1)); 
    makeCONSProf(rAddr1, pair, consPtr, pPoint);
    res = consPtr;
    i = 1;
    for (;fragPtr;fragPtr=fragPtr->n) {
      ch = ((char *)(fragPtr+1))+i;
      for (;i<fragPtr->fragmentSize;i++) {
	allocRecordMLProf(rAddr2, 2, tpair, pPoint);
	first(tpair) = convertIntToML (*ch++);
	makeCONSProf(rAddr1, tpair, consPtr, pPoint);
	second(pair) = (int) consPtr;
	pair = tpair;
      }
      i=0; /* Start with first character in the following fragments. */
    }
    makeNILProf(rAddr1, consPtr, pPoint)
    second(pair) = (int) consPtr;
  }
  return res;
}
#endif /*UNBOX_LISTS*/

/*------------------------------------------------------------------*
 * concatString: Concats str1 and str2, and returns a new string in *
 *               region rAddr.                                      *
 *------------------------------------------------------------------*/
StringDesc *concatStringProfiling(int rAddr, StringDesc *str1, StringDesc *str2, int pPoint) {

  StringDesc *res;
  StringFragment *fragPtr;

  res = allocStringProfiling(rAddr, sizeStringDefine(str1)+sizeStringDefine(str2), pPoint);
  fragPtr = &(res->sf);
  copyString(str2, &fragPtr, copyString(str1, &fragPtr, (char *) (res+1)));

  return res;
}


StringDesc *exnNameProfiling(int rAddr, int e, int pp) {
  StringDesc *ml_s;
  StringDesc *ml_empty;
  StringDesc *res;
  char *empty = "";

  ml_s = (StringDesc*)(* ( (* (int **)e) + 1 ));
  ml_empty = convertStringToMLProfiling(rAddr, empty, pp);  /* the empty ML string */
  res = concatStringProfiling(rAddr, ml_empty, ml_s, pp);   /* copy the string */

  return res;
}


#endif /*PROFILING*/




/*-------------------------------------------------------------------*
 *                Converting functions.                              *
 *-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*
 * convertStringToC:                                                 *
 *   The C string (cStr) has to be allocated.                        *
 *-------------------------------------------------------------------*/
void convertStringToC(StringDesc *mlStr, char *cStr, int cStrLen, int exn) {
  int i;
  StringFragment *fragPtr;
  char *fragCh;

  if (sizeStringDefine(mlStr) > cStrLen-1) { raise_exn(exn);
   /*
    printf("convertStringToC: String too large.\n");
    printf("ML string: ");
    printString(mlStr);
    printf("\nLength of C string: %d.\n", cStrLen);
    die("Error in convertStringToC");   
   */
  }
  else {
    for (fragPtr=&(mlStr->sf);fragPtr;fragPtr=fragPtr->n) {
      fragCh = (char *) (fragPtr+1);
      for (i=0;i<(fragPtr->fragmentSize);i++)
        *cStr++=*fragCh++;
      *cStr = '\0';
    }
  }
}

/*-------------------------------------------------------------------*
 * convertStringToML:                                                *
 *   The value rAddr is a pointer to the region where the memory     *
 *   for the ML string should be allocated.                          *
 *-------------------------------------------------------------------*/
StringDesc *convertStringToML(int rAddr, char *cStr) {

  StringDesc *res;
  StringFragment *fragPtr;
  char *fragCh;
  int i;

  res = allocString(rAddr, strlen(cStr));

  for (fragPtr=&(res->sf);fragPtr;fragPtr=fragPtr->n) {
    fragCh = (char *) (fragPtr+1);
    for (i=0;i<(fragPtr->fragmentSize);i++)
      *fragCh++=*cStr++;
  }

  return res;
}


#ifdef PROFILING
/*-------------------------------------------------------------------*
 * convertStringToML:                                                *
 *   The value rAddr is a pointer to the region where the string is  *
 *   allocated.                                                      *
 *-------------------------------------------------------------------*/
StringDesc *convertStringToMLProfiling(int rAddr, char *cStr, int pPoint) {

  StringDesc *res;
  StringFragment *fragPtr;
  char *fragCh;
  int i;

  res = allocStringProfiling(rAddr, strlen(cStr), pPoint);

  for (fragPtr=&(res->sf);fragPtr;fragPtr=fragPtr->n) {
    fragCh = (char *) (fragPtr+1);
    for (i=0;i<(fragPtr->fragmentSize);i++)
      *fragCh++=*cStr++;
  }

  return res;
}

#endif /*PROFILING*/


/* DEBUG */
void printNum(int n) {
  printf("Num: %d\n",n);
  return;
}

