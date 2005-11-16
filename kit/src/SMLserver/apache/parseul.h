#include "../../CUtils/hashmap.h"

enum ParseRV/*{{{*/
{
  Parse_OK = 0,
  Parse_ALLOCERROR = 1,
  Parse_FORMUOERROR = 2,
  Parse_FORMLOCERROR = 3,
  Parse_FORMULERROR = 4,
  Parse_FORMMAPERROR = 5,
  Parse_DUPLICATE = 6,
  Parse_INTERMALERROR = 7,
  Parse_FILEDOESNOTEXISTS = 8,
  Parse_ERROR = 9
};/*}}}*/

struct parseCtx/*{{{*/
{
  char *fileprefix;
  int fpl;
  char *mapprefix;
  int mpl;
  char *root;
  int rl;
  hashtable *uoTable;
  hashtable *smlTable;
  hashtable *ulTable;
};/*}}}*/

struct uoHashEntry
{
  unsigned long hashval;
  char *key;
};

struct char_charHashEntry
{
  unsigned long hashval;
  char *key;
  char *val;
};

unsigned long uoHashEntry_HashFun(void *);

int uoHashEntry_EqualFun(void *, void *);

unsigned long char_charHashFun (void *);

int char_charEqualFun(void *, void *);

int recurseParse(struct parseCtx *ctx, char *filename);

