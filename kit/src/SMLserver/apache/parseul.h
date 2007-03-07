#ifndef PARSEUL_H
#define PARSEUL_H
#include "../../Runtime/LoadKAM.h"
#include "../../CUtils/polyhashmap.h"

enum ParseRV/*{{{*/
{
  Parse_OK = 0,
  Parse_ALLOCERROR = 1,
  Parse_FORMUOERROR = 2,
  Parse_FORMLOCERROR = 3,
  Parse_FORMULERROR = 4,
  Parse_FORMMAPERROR = 5,
  Parse_DUPLICATE = 6,
  Parse_INTERNALERROR = 7,
  Parse_FILEDOESNOTEXISTS = 8,
  Parse_ERROR = 9
};/*}}}*/

DECLARE_NHASHMAP(parseul,char *, char *, const, const)

struct parseCtx/*{{{*/
{
  void *ctx;
  char *fileprefix;
  int fpl;
  char *mapprefix;
  int mpl;
  char *root;
  int rl;
  Interp *interp;
  parseul_hashtable_t *uoTable;
  parseul_hashtable_t *smlTable;
  parseul_hashtable_t *ulTable;
};/*}}}*/

/*
struct uoHashEntry
{
  unsigned long hashval;
  char *key;
};

struct char_charHashEntry
{
//  unsigned long hashval;
  char *key;
  char *val;
}; */

/*unsigned long uoHashEntry_HashFun(void *);

int uoHashEntry_EqualFun(void *, void *);

unsigned long char_charHashFun (void *);

int char_charEqualFun(void *, void *); */

int recurseParse(struct parseCtx *ctx, const char *filename);

void clearSmlMap(parseul_hashtable_t *); 

void clearPCtx(struct parseCtx *);

void printSmlTable(parseul_hashtable_t *, void *);

#endif // PARSEUL_H
