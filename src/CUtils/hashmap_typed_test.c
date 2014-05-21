#include <string.h>
#include <stdio.h>
#include "hashmap_typed.h"

DECLARE_HASHMAP(stringToIntMap,char*,int)

DEFINE_HASHMAP(stringToIntMap,char*,int)

void
pr(char* k,int v)
{
  printf("%s : %d\n",k,v);
}

int
streq(char* s1,char* s2)
{
  if ( strcmp(s1,s2) == 0 )
    return 1;
  return 0;
}

int
main(void)
{
  stringToIntMap m;
  char* k1 = "Martin";
  char* k2 = "Carsten";
  int r, v;
  r = -1; 
  v = -1;
  m = new_stringToIntMap(charhashfunction,streq);
  stringToIntMap_upd(m,k1,3);
  stringToIntMap_upd(m,k2,8);
  stringToIntMap_apply(m,pr);
  r = stringToIntMap_find(m,"Carsten",&v);
  printf("r = %d; v = %d\n",r,v);
  return 0;
}
