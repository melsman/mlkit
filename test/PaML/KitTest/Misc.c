
#define C_STRING_LENGTH 4096    /* Maximal length of C-string. */
char cString[C_STRING_LENGTH];

typedef struct stringFragment {
  unsigned int fragmentSize;  /* Size of this fragment. */
  struct stringFragment * n;  /* Ptr. to next fragment in string. */
} StringFragment;

typedef struct stringDesc {
  unsigned int stringSize; /* Total size of string. */
  StringFragment sf;       /* StringDesc contains the first fragment and not a ptr. to it. */
} StringDesc;

#define sizeStringDefine(str)    ((((StringDesc *)str)->stringSize) >> 6) /* Remove stringtag. We do not tag the size. */

#define C_STRING_LENGTH 4096    /* Maximal length of C-string. */

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
  //  fflush(stdout);
}

Int32 terminate (Int32 status) 
{ 
  exit (status); 
}
