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
main(void)
{
  char* k1 = "Martin";
  char* k2 = "Carsten";
  stringToIntMap m = new_stringToIntMap(charhashfunction,(int(*)(char*,char*))strcmp);
  stringToIntMap_upd(m,k1,3);
  stringToIntMap_upd(m,k2,8);
  stringToIntMap_apply(m,pr);
  return 0;
}
