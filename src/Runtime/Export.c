#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "String.h"
#include "../CUtils/polyhashmap.h"
#include "../CUtils/hashfun.h"
#include "Export.h"
#include "CommandLine.h"

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
    exportmap = exportmap_new();
    if (!exportmap) return;
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

long
callExportFun(const char *fun, long i)
{
  long res = -1;
#ifdef ENABLE_GC
  long disable_gc_save = disable_gc;
  disable_gc = 0;
#endif
  //printf("callExportFun %s\n", fun);
  if (!exportmap) return res;
  const void *f1;
  long (*f2)(long);
  if (exportmap_find(exportmap, fun, &f1) == hash_OK) {
    f2 = f1;
    res = ((*f2)(i));
  }
#ifdef ENABLE_GC
  disable_gc = disable_gc_save;
#endif
  return res;
}
