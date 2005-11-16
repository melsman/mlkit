#include "../../CUtils/hashmap.h"

enum ParseRV
{
  Parse_OK = 0,
  Parse_ALLOCERROR = 1,
  Parse_DUPLICATE = 4,
  Parse_INTERMALERROR = 6,
  Parse_FILEDOESNOTEXISTS = 7,
  Parse_ERROR = 8
};

struct parseCtx
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
};

unsigned long uoHashEntry_HashFun(void *);

int uoHashEntry_EqualFun(void *, void *);

unsigned long char_charHashFun (void *);

int char_charEqualFun(void *, void *);

int recurseParse(struct parseCtx *ctx, char *filename);

