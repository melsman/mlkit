
#include <string.h>
#include <stdlib.h>
#include "String.h"
#include "Region.h"
#include "Tagging.h"
#include <dlfcn.h>
#include "../CUtils/polyhashmap.h"
#include "../CUtils/hashfun.h"
#include "Locks.h"
#include "Dlsym.h"

uintptr_t
REG_POLY_FUN_HDR(sml_dlopen,uintptr_t pair, Region s, String file1, size_t flags1)
{
  char *file = file1->data;
  char *c;
  void *p;
  int flags = convertIntToC(flags1);
  int use_lazy = RTLD_NOW;
  int use_global = RTLD_LOCAL;
  char *myfile = file;
  if (flags & 0x1) use_lazy = RTLD_LAZY;
  if (flags & 0x2) use_global = RTLD_GLOBAL;
  if (flags & 0x4) myfile = NULL;
  dlerror();
  p = dlopen(myfile, use_global | use_lazy);
  c = dlerror();
  first(pair) = (uintptr_t) p;
  if (c)
  {
    second(pair) = (uintptr_t) REG_POLY_CALL(convertStringToML,s, c);
  } else
  {
    second(pair) = 0;
  }
  return pair;
}

static int
mystreq(const char *a, const char *b)
{
  return (!strcmp(a,b));
}

DECLARE_NHASHMAP(dynamic_function_res_map, void *, char *, const, const)
DEFINE_NHASHMAP(dynamic_function_res_map, charhashfunction, mystreq)

static dynamic_function_res_map_hashtable_t *fnmap = NULL;

String
REG_POLY_FUN_HDR(resolveFun,Region sAddr, String our_name, String cname, void *libhandle)
{
  char *c;
  const void *fp = NULL;
  LOCK_LOCK(FUNCTIONTABLEMUTEX);
  if (fnmap == NULL) fnmap = dynamic_function_res_map_new();
  if (fnmap == NULL)
  {
    LOCK_UNLOCK(FUNCTIONTABLEMUTEX);
    return (String) REG_POLY_CALL(convertStringToML,sAddr, "Allocation Error in __FILE__:__LINE__");
  }
  if (dynamic_function_res_map_find(fnmap, our_name->data, &fp) == hash_DNE)
  {
    dlerror();
    fp = dlsym(libhandle, cname->data);
    c = dlerror();
    if (c)
    {
      return (String) REG_POLY_CALL(convertStringToML,sAddr,c);
    }
    dynamic_function_res_map_update(fnmap, our_name->data, fp);
    LOCK_UNLOCK(FUNCTIONTABLEMUTEX);
    return NULL;
  }
  else
  { // hash_OK
    LOCK_UNLOCK(FUNCTIONTABLEMUTEX);
    return (String) REG_POLY_CALL(convertStringToML,sAddr, "Dynamic function already resolved");
  }
  LOCK_UNLOCK(FUNCTIONTABLEMUTEX);
  return NULL;
}

size_t
isResolvedFun (const char *name)
{
  const void *fp = NULL;
  if (!fnmap) return 0;
  if (dynamic_function_res_map_find(fnmap, name, &fp) == hash_DNE)
  {
    return 0;
  }
  else
  {
    return 1;
  }
  return 0;
}

void
localResolveLibFnAuto(const void **fp, const char *fname)
{
  const void *myfp = NULL;
  LOCK_LOCK(FUNCTIONTABLEMUTEX);
  if (fnmap == NULL) return;
  if (dynamic_function_res_map_find(fnmap, fname, &myfp) == hash_DNE)
  {
    LOCK_UNLOCK(FUNCTIONTABLEMUTEX);
    return;
  }
  else
  {
    *fp = myfp;
    LOCK_UNLOCK(FUNCTIONTABLEMUTEX);
    return;
  }
  LOCK_UNLOCK(FUNCTIONTABLEMUTEX);
  return;
}

void
localResolveLibFnManual(const void **fp, String fname)
{
  localResolveLibFnAuto(fp, fname->data);
}

String
REG_POLY_FUN_HDR(fromCtoMLstring,Region sAddr, char *cs)
{
  return (String) REG_POLY_CALL(convertStringToML,sAddr,cs);
}
