
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <alloca.h>
#include "../../CUtils/polyhashmap.h"
#include "parseul.h"
#include "parseFuncs.h"
#include "ul.tab.h"
#include "plog.h"
#include "mod_sml.h"

DEFINE_NHASHMAP(parseul,charhashfunction,charEqual)

char *
contractPath1(char *path, char *lastSlash)/*{{{*/
{
  char *b, c;
  int len = strlen(path);
//  printf("contractPath1: %p, %p, %s, %i\n", path, lastSlash, path, lastSlash - path);
  if (!*path) return path;
  if (*path == '/')
  {
    memmove(path, path+1, len);
    return contractPath1(path, lastSlash);
  }
  if (path[0] == '.' && path[1] == '.' && path[2] == '/')
  {
    if (lastSlash == NULL)
    {
      return NULL;
    }
    memmove(lastSlash, path+2, len - 1);
    return lastSlash + 1;
  }
  if (*path == '.' && path[1] == '/')
  {
    memmove(path, path+2, len - 1);
    return contractPath1(path, lastSlash);
  }
  b = path - 1;
  while ((c = *path))
  {
    if (c == '/') 
    {
      b = contractPath1(path+1, b);
      if (b == NULL) return NULL;
      return contractPath1(b, lastSlash);
    }
    path++;
  }
  return path;
}/*}}}*/

char *
contractPath(char *path)/*{{{*/
{
  char *res;
  if (path && path[0] == '/')
  {
    res = contractPath1(path + 1, NULL);
//    printf("contractPath %s\n", path);
    return res;
  }
  else
  {
    res = contractPath1(path, NULL);
//    printf("contractPath %s\n", path);
    return res;
  }
}/*}}}*/

char *
addMlb(char *context, const char *in)/*{{{*/
{
  char c, *p, *ls, *tmp;
  p = context;
  ls = NULL;
  while ((c = *p))
  {
    if (c == '/')
    {
      ls = p;
    }
    p++;
  }
  if (ls == NULL) ls = context;
  tmp = alloca(strlen(ls) + 1);
  if (!tmp) return NULL;
  strcpy(tmp, ls);
  strcpy(ls,in);
  ls += strlen(in);
  strcpy(ls,tmp);
  return ls;
}/*}}}*/

// trashes uo and mlop
char *
formSml(char *uo, const char *fileprefix, int fpl, const char *mapprefix, int mpl,/*{{{*/
        char *mlop, const char *root, int rootLength, char *res)
{
//  printf("formSml: %s, %s, %s, %s %s\n", uo, fileprefix, mapprefix, mlop, root);
  if (mlop == NULL)
  {
    if (uo[0] == '/')
    {
      if (strstr(uo,fileprefix) == uo)
      {
        strcpy(res,mapprefix);
        strcpy(res+mpl, "/");
        strcpy(res+mpl+1, uo+fpl);
        return contractPath(res);
      }
      else
      {
        return NULL;
      }
    }
    else
    {
      strcpy(res,uo);
      if (!contractPath(res)) return NULL;
      strcpy(uo,res);
      strcpy(res,mapprefix);
      strcpy(res+mpl, "/");
      strcpy(res+mpl+1, uo);
      return contractPath(res);
    }
  }
  else
  {
    if (mlop[0] == '/')
    {
      strcpy(res,mlop);
      if (!contractPath(res)) return NULL;
      strcpy(mlop,res);
      strcpy(res,root);
      strcpy(res+rootLength,mlop);
      return contractPath(res);
    }
    else
    {
      strcpy(res,mlop);
      if (!contractPath(res)) return NULL;
      strcpy(mlop,res);
      strcpy(res,mapprefix);
      strcpy(res+mpl,"/");
      strcpy(res+mpl+1,mlop);
      return contractPath(res);
    }
  }
}/*}}}*/

const char *mlb = "/MLB/SMLserver";

char *
formUoUl(const char *uo, const char *fileprefix, int fpl, char *res)/*{{{*/
{
  char *r;
  if (uo[0] == '/')
  {
    strcpy(res, uo);
  }
  else
  {
    strcpy(res,fileprefix);
    res[fpl] = '/';
    res[fpl+1] = 0;
    strcpy(res + fpl+1, uo);
  }
  r = contractPath(res);
//  printf("formUoUl: %s, %s -> %s\n", uo, fileprefix, res);
  return r;
}/*}}}*/

char *
formUo(const char *uo, const char *fileprefix, int fpl, char *res)/*{{{*/
{
  formUoUl(uo,fileprefix,fpl,res);
  return res;
//  return addMlb(res,mlb);
}/*}}}*/

char *
formUl(const char *ul, const char *fileprefix, int fpl, char *res)/*{{{*/
{
  return formUoUl(ul, fileprefix, fpl, res);
}/*}}}*/

char *
formMap(const char *loc, const char *mapprefix, int mpl, char *res)/*{{{*/
{
  int i;
  if (loc[0] == '/')
  {
    strcpy(res, loc);
    return contractPath(res);
  }
  else
  {
    strcpy(res,mapprefix);
    i = mpl;
    if (res[mpl - 1] != '/')
    {
      res[mpl] = '/';
      i++;
    }
    strcpy(res+i, loc);
    return contractPath(res);
  }
}/*}}}*/

int
path(const char * restrict file)/*{{{*/
{
  int i,ls;
  ls = -1;
  for (i = 0; file[i]; i++)
  {
    if (file[i] == '/') ls = i;
  }
  return ls;
}/*}}}*/

#if 0
unsigned long
uoHashEntry_HashFun(void *key1)/*{{{*/
{
  struct uoHashEntry *key = (struct uoHashEntry *) key1;
  return key->hashval;
}/*}}}*/

int
uoHashEntry_EqualFun(void *key1, void *key2)/*{{{*/
{
  struct uoHashEntry *he1;
  struct uoHashEntry *he2;
  he1 = (struct uoHashEntry *) key1;
  he2 = (struct uoHashEntry *) key2;
  return (he1->hashval == he2->hashval && !strcmp(he1->key, he2->key));
}/*}}}*/

unsigned long
char_charHashFun (void *key1)/*{{{*/
{
  struct char_charHashEntry *he = (struct char_charHashEntry *) key1;
  return he->hashval;
}/*}}}*/

int
char_charEqualFun(void *key1, void *key2)/*{{{*/
{
  struct char_charHashEntry *he1;
  struct char_charHashEntry *he2;
  he1 = (struct char_charHashEntry *) key1;
  he2 = (struct char_charHashEntry *) key2;
  return (he1->hashval == he2->hashval && !strcmp(he1->key, he2->key));
}/*}}}*/
#endif

int
toUlHashTable(void *pctx1, const char *ul, int ulLength, const char *loc, int locLength)/*{{{*/
{
  struct parseCtx *pctx, rpctx;
//  struct char_charHashEntry *he, he1;
  char *tmp, *tmp2;
  const char *tmp3;
//  void **r;
  int i;
  if (!ul) return Parse_ALLOCERROR;
  if (!loc) return Parse_ALLOCERROR;
  tmp = (char *) alloca(ulLength + 1 + locLength + 1);
  if (!tmp) return Parse_ALLOCERROR;
  strncpy(tmp, ul, ulLength);
  tmp[ulLength] = 0;
  ul = tmp;
  tmp += ulLength + 1;
  strncpy(tmp, loc, locLength);
  tmp[locLength] = 0;
  loc = tmp;
  pctx = (struct parseCtx *) pctx1;
  tmp = (char *) alloca(ulLength+1+pctx->fpl + 1);
  tmp2 = (char *) alloca(locLength+1+pctx->mpl + 1);
//  printf("toUlHashTable: %s, %s\n", ul, loc);
  if (!tmp || !tmp2) return Parse_ALLOCERROR;
  if (!formUl(ul,pctx->fileprefix, pctx->fpl, tmp)) return Parse_FORMULERROR;
  if (!formMap(loc,pctx->mapprefix, pctx->mpl, tmp2)) return Parse_FORMMAPERROR;
  char * restrict key, * restrict val;
  if (parseul_find(pctx->ulTable, tmp, &tmp3) == hash_DNE)
  {
    key = malloc(strlen(tmp) + 1 + strlen(tmp2) + 3);
    if (!key) return Parse_ALLOCERROR;
    val = key + (strlen(tmp)) + 1;
    strcpy(key, tmp);
    strcpy(val, tmp2);
    if (parseul_update(pctx->ulTable, key, val) != hash_OK) return Parse_ALLOCERROR;
  }
  else
  {
	plog2s(tmp, " already visited", pctx->ctx);
    return Parse_DUPLICATE;
  }
  tmp = (char *) alloca(strlen(key) + 1 + strlen(val) + 3);
  if (!tmp) return Parse_ALLOCERROR;
  tmp2 = key + strlen(key) + 1;
  i = path(key);
  if (i == -1) return Parse_INTERNALERROR;
  strncpy(tmp, key, i+1);
  tmp[i+1] = 0;
  strcpy(tmp2, val);
  rpctx.interp = pctx->interp;
  rpctx.ctx = pctx->ctx;
  rpctx.fileprefix = tmp;
  rpctx.fpl = strlen(rpctx.fileprefix);
  rpctx.mapprefix = tmp2;
  rpctx.mpl = strlen(rpctx.mapprefix);
  rpctx.root = pctx->root;
  rpctx.rl = pctx->rl;
  rpctx.uoTable = pctx->uoTable;
  rpctx.smlTable = pctx->smlTable;
  rpctx.ulTable = pctx->ulTable;
//  plog5s("Recursing into %s\n  with fp: %s, mp: %s, root: %s\n", he->key,rpctx.fileprefix, rpctx.mapprefix, rpctx.root,pctx->ctx);
  i = recurseParse(&rpctx, key);
  plog2s("Done with ", key, pctx->ctx);
  return i;
}/*}}}*/

int 
toSmlHashTable(void *pctx1, char *uo, int uoLength, char *mlop, int mlopLength)/*{{{*/
{
  struct parseCtx *pctx;
  char *tmp, *tmp2;
  const char *tmp3;
  if (!uo) return Parse_ALLOCERROR;
  tmp = (char *) alloca(uoLength + mlopLength + 2);
  if (!tmp) return Parse_ALLOCERROR;
  strncpy(tmp, uo, uoLength);
  tmp[uoLength] = 0;
  uo = tmp;
  if (mlop)
  {
    tmp = tmp + uoLength + 1;
    strncpy(tmp, mlop, mlopLength);
    tmp[mlopLength] = 0;
    mlop = tmp;
  }
  pctx = (struct parseCtx *) pctx1;
  tmp = (char *) alloca(uoLength + 2 + pctx->fpl + strlen(mlb));
  tmp2 = (char *) alloca(uoLength + 2 + pctx->fpl + mlopLength + pctx->rl + pctx->mpl);
//  printf("toSmlHashTable: %s, %s\n", uo, mlop);
  if (!tmp || !tmp2) return Parse_ALLOCERROR;
  if (!formUo(uo, pctx->fileprefix, pctx->fpl, tmp)) return Parse_FORMUOERROR;
  if (!formSml(uo, pctx->fileprefix, pctx->fpl, pctx->mapprefix, pctx->mpl,
          mlop, pctx->root, pctx->rl, tmp2)) 
  {
    // Skipping not visible sml files
    return Parse_OK;
  }
  char * restrict key;
  char * restrict val;
  if (parseul_find(pctx->smlTable, tmp2, &tmp3) == hash_DNE)
  {
    key = (char * restrict) malloc(strlen(tmp) + strlen(tmp2) + 2);
    if (!key) return Parse_ALLOCERROR;
    strcpy(key,tmp2);
    val = key + strlen(key) + 1;
    strcpy(val,tmp);
    plog4s("Mapping ", key, " ", val, pctx->ctx);
    parseul_update(pctx->smlTable, key, val);
  }
  else 
  {
    return Parse_DUPLICATE;
  }
  return Parse_OK;
}/*}}}*/

int
extendInterp (void *pctx1, const char *uo, int len)/*{{{*/
{
  struct parseCtx *pctx;
//  struct uoHashEntry *he, he1;
//  void *r;
  char *tmp;
  char * restrict key;
  const char * val;
  if (!uo) return Parse_ALLOCERROR;
  tmp = (char *) alloca (len + 1);
  if (!tmp) return Parse_ALLOCERROR;
  strncpy(tmp, uo, len);
  tmp[len] = 0;
  uo = tmp;
  pctx = (struct parseCtx *) pctx1;
  tmp = (char *) alloca(len+1+pctx->fpl);
//  printf("extendInterp %s\n", uo);
  if (!tmp) return Parse_ALLOCERROR;
  if (!formUo(uo, pctx->fileprefix, pctx->fpl, tmp)) return Parse_FORMUOERROR;
  if (parseul_find(pctx->uoTable, tmp, &val) == hash_DNE)
  {
    key = (char * restrict) malloc(strlen(tmp) + 1);
    if (!key) return Parse_ALLOCERROR;
    strcpy(key, tmp);
    parseul_update(pctx->uoTable, key, NULL);
    if (interpLoadExtend(pctx->interp, tmp, pctx->ctx)) return Parse_ERROR;
    plog2s("Extending interpreter with ", tmp, pctx->ctx);
  } // if already in the interpreter then skip
  else plog2s("Skipping ", tmp, pctx->ctx);
  return Parse_OK;
}/*}}}*/

void
yyerror(YYLTYPE *loc, void *ctx, const char *msg)/*{{{*/
{
  struct parseCtx *pctx = (struct parseCtx *) ctx;
  plog2s("Parse Error: ", msg, pctx->ctx);
  return;
}/*}}}*/

void
myfree(const char *key, const char *value)/*{{{*/
{
  free((void *) key);
  return;
}/*}}}*/

void
clearSmlMap(parseul_hashtable_t *t)/*{{{*/
{
  if (t)
  {
    parseul_Apply(t, myfree);
    parseul_close(t);
    free(t);
  }
  return;
}/*}}}*/

void
clearPCtx(struct parseCtx *t)/*{{{*/
{
  if (t->uoTable)
  {
    parseul_Apply(t->uoTable, myfree);
    parseul_close(t->uoTable);
    free(t->uoTable);
    t->uoTable = NULL;
  }
  if (t->ulTable)
  {
    parseul_Apply(t->ulTable, myfree);
    parseul_close(t->ulTable);
    free(t->ulTable);
    t->ulTable = NULL;
  }
  if (t->smlTable)
  {
    parseul_Apply(t->smlTable, myfree);
    parseul_close(t->smlTable);
    free(t->smlTable);
    t->smlTable = NULL;
  }
  return;
}/*}}}*/

static void *
ppSmlTable(const char *key, const char *value, void *ctx)/*{{{*/
{
  plog4s("apsml: ", key, " Maps to ", value, ctx);
  return ctx;
}/*}}}*/

void
printSmlTable(parseul_hashtable_t *t, void *ctx)/*{{{*/
{
  if (!t) return;
  parseul_Fold(t, ppSmlTable, ctx);
  return;
}/*}}}*/
