#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "String.h"
#include "../CUtils/hashmap.h"

static hashtable *exportmap = NULL;

struct e_map
{
  char *fun;
  unsigned long hashval;
};

static unsigned long
hashfun(void *e1)
{
  struct e_map *e = (struct e_map *) e1;
  return e->hashval;
}

static int
hash_eq(void *e1, void *e2)
{
  struct e_map *ea = (struct e_map *) e1;
  struct e_map *eb = (struct e_map *) e2;
  return (ea->hashval == eb->hashval && !(strcmp(ea->fun, eb->fun)));
}

void
sml_regCfuns(String name,void *f)
{
  struct e_map e, *e1;
  void *f1;
  if (!exportmap) 
  {
    exportmap = (hashtable *) malloc(sizeof (hashtable));
    if (!exportmap) return;
    hashinit(exportmap, hashfun, hash_eq);
  }
  e.fun = &(name->data);
  e.hashval = charhashfunction(e.fun);
  if (hashfind(exportmap, &e, &f1) == hash_DNE)
  {
    e1 = (struct e_map *) malloc(sizeof(struct e_map) + strlen(e.fun) + 1);
    if (!e1) return;
    e1->fun = ((char *) e1) + sizeof(struct e_map);
    e1->hashval = e.hashval;
    strcpy(e1->fun, e.fun);
    hashinsert(exportmap, e1, f);
  }
  return;
}

int
callExportFun(char *fun, int i)
{
  if (!exportmap) return -1;
  struct e_map e;
  void *f1;
  int (*f2)(int);
  e.fun = fun;
  e.hashval = charhashfunction(e.fun);
  if (hashfind(exportmap, &e, &f1) == hash_OK)
  {
    f2 = f1;
    return ((*f2)(i));
  }
  else
  {
    return -1;
  }
}
