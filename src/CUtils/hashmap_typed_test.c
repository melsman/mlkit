#include <string.h>
#include <stdio.h>
#include "hashmap_typed.h"

DECLARE_HASHMAP(stringToIntMap,char*,long int)

DEFINE_HASHMAP(stringToIntMap,char*,long int)

void
pr(char* k,long int v)
{
  printf("%s : %ld\n",k,v);
}

int
streq(char* s1,char* s2)
{
  if ( strcmp(s1,s2) == 0 )
    return 1;
  return 0;
}

// char * -> unsigned long
unsigned long
strhash (char *s)
{
  unsigned long result = 0;
  unsigned char *k = (unsigned char *) s;
  while (*k)
    { // Will not overflow as the type is unsigned
      result = 31 * result + *k++;
    }
  return result;
}

int
main(void)
{
  stringToIntMap m;
  char* k1 = "Martin";
  char* k2 = "Carsten";
  long int r, v;
  r = -1;
  v = -1;
  m = new_stringToIntMap(charhashfunction,streq);
  stringToIntMap_upd(m,k1,3);
  stringToIntMap_upd(m,k2,8);
  stringToIntMap_apply(m,pr);
  r = stringToIntMap_find(m,"Carsten",&v);
  printf("r = %ld; v = %ld\n",r,v);
  return 0;
}
