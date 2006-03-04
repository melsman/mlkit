
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <alloca.h>
#include "../../CUtils/hashmap.h"
#include "parseul.h"
#include "parseFuncs.h"
#include "ul.tab.h"
#include "plog.h"

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
formSml(char *uo, char *fileprefix, int fpl, char *mapprefix, int mpl,/*{{{*/
        char *mlop, char *root, int rootLength, char *res)
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
formUoUl(char *uo, char *fileprefix, int fpl, char *res)/*{{{*/
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
formUo(char *uo, char *fileprefix, int fpl, char *res)/*{{{*/
{
  formUoUl(uo,fileprefix,fpl,res);
  return res;
//  return addMlb(res,mlb);
}/*}}}*/

char *
formUl(char *ul, char *fileprefix, int fpl, char *res)/*{{{*/
{
  return formUoUl(ul, fileprefix, fpl, res);
}/*}}}*/

char *
formMap(char *loc, char *mapprefix, int mpl, char *res)/*{{{*/
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
path(char *file)/*{{{*/
{
  int i,ls;
  ls = -1;
  for (i = 0; file[i]; i++)
  {
    if (file[i] == '/') ls = i;
  }
  return ls;
}/*}}}*/

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

int
toUlHashTable(void *pctx1, char *ul, int ulLength, char *loc, int locLength)/*{{{*/
{
  struct parseCtx *pctx, rpctx;
  struct char_charHashEntry *he, he1;
  char *tmp, *tmp2, *tmp3;
  void **r;
  int i;
  if (!ul) return Parse_ALLOCERROR;
  if (!loc) return Parse_ALLOCERROR;
  tmp = (char *) alloca(ulLength + 1 + locLength + 1);
  if (!tmp) return Parse_ALLOCERROR;
  strncpy(tmp, ul, ulLength);
  ul = tmp;
  tmp[ulLength] = 0;
  tmp += ulLength + 1;
  strncpy(tmp, loc, locLength);
  loc = tmp;
  loc[locLength] = 0;
  pctx = (struct parseCtx *) pctx1;
  tmp = (char *) alloca(ulLength+1+pctx->fpl + 1);
  tmp2 = (char *) alloca(locLength+1+pctx->mpl + 1);
//  printf("toUlHashTable: %s, %s\n", ul, loc);
  if (!tmp || !tmp2) return Parse_ALLOCERROR;
  if (!formUl(ul,pctx->fileprefix, pctx->fpl, tmp)) return Parse_FORMULERROR;
  if (!formMap(loc,pctx->mapprefix, pctx->mpl, tmp2)) return Parse_FORMMAPERROR;
  he1.key = tmp;
  he1.hashval = charhashfunction(he1.key);
  r = (void **) (&tmp3);
  if (hashfind(pctx->ulTable, &he1, r) == hash_DNE)
  {
    he = (struct char_charHashEntry *) malloc(sizeof(struct char_charHashEntry) + strlen(tmp) + 1 +
                                                     strlen(tmp2) + 3);
    if (!he) return Parse_ALLOCERROR;
    he->key = (char *) (he+1);
    he->val = he->key + strlen(tmp) + 1;
    strcpy(he->key, tmp);
    strcpy(he->val, tmp2);
    he->hashval = he1.hashval;
    if (hashupdate(pctx->ulTable, he, he->val) != hash_OK) return Parse_ALLOCERROR;
  }
  else
  {
	plog2s(tmp, " already visited", pctx->ctx);
    return Parse_DUPLICATE;
  }
  tmp = (char *) alloca(strlen(he->key) + 1 + strlen(he->val) + 3);
  if (!tmp) return Parse_ALLOCERROR;
  tmp2 = he->key + strlen(he->key) + 1;
  i = path(he->key);
  if (i == -1) return Parse_INTERMALERROR;
  strncpy(tmp, he->key, i+1);
  tmp[i+1] = 0;
  strcpy(tmp2, he->val);
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
  i = recurseParse(&rpctx, he->key);
  plog2s("Done with ", he->key, pctx->ctx);
  return i;
}/*}}}*/

int 
toSmlHashTable(void *pctx1, char *uo, int uoLength, char *mlop, int mlopLength)/*{{{*/
{
  struct parseCtx *pctx;
  struct char_charHashEntry *he, he1;
  char *tmp, *tmp2, *tmp3;
  if (!uo) return Parse_ALLOCERROR;
  tmp = (char *) alloca(uoLength + mlopLength + 2);
  if (!tmp) return Parse_ALLOCERROR;
  strncpy(tmp, uo, uoLength);
  tmp[uoLength] = 0;
  uo = tmp;
  if (mlop)
  {
    tmp = uo + uoLength + 1;
    strncpy(tmp, mlop, mlopLength);
    mlop = tmp;
    mlop[mlopLength] = 0;
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
  he1.key = tmp2;
  he1.hashval = charhashfunction(he1.key);
  if (hashfind(pctx->smlTable, &he1, (void **) &tmp3) == hash_DNE)
  {
    he = (struct char_charHashEntry *) malloc(sizeof (struct char_charHashEntry) +
                                              strlen(tmp) + strlen(tmp2) + 2);
    if (!he) return Parse_ALLOCERROR;
    he->key = (char *)(he+1);
    strcpy(he->key, he1.key);
    he->hashval = he1.hashval;
    he->val = he->key + strlen(he->key) + 1;
    strcpy(he->val,tmp);
    plog4s("Mapping ", he->key, " ", he->val, pctx->ctx);
    hashupdate(pctx->smlTable, he, he->val);
  }
  else 
  {
    return Parse_DUPLICATE;
  }
  return Parse_OK;
}/*}}}*/

int
extendInterp (void *pctx1, char *uo, int len)/*{{{*/
{
  struct parseCtx *pctx;
  struct uoHashEntry *he, he1;
  void *r;
  char *tmp;
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
  he1.key = tmp;
  he1.hashval = charhashfunction(tmp);
  if (hashfind(pctx->uoTable, &he1, &r) == hash_DNE)
  {
    he = (struct uoHashEntry *) malloc(sizeof (struct uoHashEntry) + strlen(tmp) + 1);
    if (!he) return Parse_ALLOCERROR;
    he->key = (char *)(he+1);
    strcpy(he->key, tmp);
    he->hashval = he1.hashval;
    hashupdate(pctx->uoTable, he, NULL);
    if (interpLoadExtend(pctx->interp, tmp)) return Parse_ERROR;
    plog2s("Extending interpreter with ", tmp, pctx->ctx);
  } // if already in the interpreter then skip
  else plog2s("Skipping ", tmp, pctx->ctx);
  return Parse_OK;
}/*}}}*/

void
yyerror(YYLTYPE *loc, void *ctx, const char *msg)/*{{{*/
{
  struct parseCtx *pctx = (struct parseCtx *) ctx;
  plog2s("Parse Error: ", (char *) msg, pctx->ctx);
  return;
}/*}}}*/

void
myfree(void *key, void *value)/*{{{*/
{
  free(key);
  return;
}/*}}}*/

void
clearSmlMap(hashtable *t)/*{{{*/
{
  if (t)
  {
    hashApply(t, myfree);
    hashclose(t);
    free(t);
  }
  return;
}/*}}}*/

void
clearPCtx(struct parseCtx *t)/*{{{*/
{
  if (t->uoTable)
  {
    hashApply(t->uoTable, myfree);
    hashclose(t->uoTable);
    free(t->uoTable);
    t->uoTable = NULL;
  }
  if (t->ulTable)
  {
    hashApply(t->ulTable, myfree);
    hashclose(t->ulTable);
    free(t->ulTable);
    t->ulTable = NULL;
  }
  if (t->smlTable)
  {
    hashApply(t->smlTable, myfree);
    hashclose(t->smlTable);
    free(t->smlTable);
    t->smlTable = NULL;
  }
  return;
}/*}}}*/

static void *
ppSmlTable(void *key1, void *value1, void *ctx)/*{{{*/
{
  char *value;
  struct char_charHashEntry *he;
  value = (char *) value1;
  he = (struct char_charHashEntry *) key1;
  plog4s("apsml: ", he->key, " Maps to ", value, ctx);
  return ctx;
}/*}}}*/

void
printSmlTable(hashtable *t, void *ctx)/*{{{*/
{
  if (!t) return;
  hashFold(t, ppSmlTable, ctx);
  return;
}/*}}}*/
