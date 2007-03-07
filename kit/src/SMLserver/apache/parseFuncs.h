
#include "ul.tab.h"
#include "string.h"

struct tokens
{
  char *ptr;
  int len;
};

union hat
{
  struct tokens t;
  void *p;
};

#define YYSTYPE union hat

int toUlHashTable(void *, const char *, int, const char *, int);

int toSmlHashTable(void *, char *, int, char *, int);

int extendInterp(void *, const char *, int);

int yylex (YYSTYPE *, YYLTYPE *);

int yyparse (void *); 

void yyerror(YYLTYPE *, void *, const char *);

#define YY_DECL int yylex (YYSTYPE *lvalp, YYLTYPE *llocp)
