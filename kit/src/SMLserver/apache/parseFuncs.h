
#include "ul.tab.h"

struct data
{
  void *ptr;
  int len;
};

#define YYSTYPE struct data

int toUlHashTable(void *, char *ul, int ulLength, char *loc, int locLength);

int toSmlHashTable(void *, char *uo, int uoLength, char *mlop, int mlopLength);

int extendInterp(void *, char *, int);

int yylex (YYSTYPE *, YYLTYPE *);

#define YY_DECL int yylex (YYSTYPE *lvalp, YYLTYPE *llocp)
