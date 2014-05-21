#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "String.h"
#include "../CUtils/polyhashmap.h"
#include "../CUtils/hashfun.h"
#include "Export.h"


static int
charEqual(const char *a, const char *b)
{ 
    return (strcmp(a,b) ? 0 : 1);
} 

DECLARE_NHASHMAP(exportmap,void *, char *, const, const)
DEFINE_NHASHMAP(exportmap,charhashfunction,charEqual)

static exportmap_hashtable_t *exportmap = NULL;

void
sml_regCfuns(String name,void *f)
{
  const char *e;
  char *e1;
  const void *f1;
  //  printf("sml_regCfuns %s\n", &(name->data));
  if (!exportmap) 
  {
    exportmap = (exportmap_hashtable_t *) malloc(sizeof (exportmap_hashtable_t));
    if (!exportmap) return;
    exportmap_init(exportmap);
  }
  e = &(name->data);
  if (exportmap_find(exportmap, e, &f1) == hash_DNE)
  {
    e1 = (char *) malloc(strlen(e) + 1);
    if (!e1) return;
    strcpy(e1, &(name->data));
    exportmap_insert(exportmap, e1, f);
  }
  return;
}

int
callExportFun(const char *fun, int i)
{
  //  printf("callExportFun %s\n", fun);
  if (!exportmap) return -1;
  const void *f1;
  int (*f2)(int);
  if (exportmap_find(exportmap, fun, &f1) == hash_OK)
  {
    f2 = f1;
    return ((*f2)(i));
  }
  else
  {
    return -1;
  }
}
