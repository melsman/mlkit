
#include "ul.tab.h"
#include "string.h"

#define YYSTYPE char *

int toUlHashTable(void *, char *, char *);

int toSmlHashTable(void *, char *, char *);

int extendInterp(void *, char *);

int yylex (YYSTYPE *, YYLTYPE *);

int yyparse (void *); 

void yyerror(YYLTYPE *, void *, const char *);

#define YY_DECL int yylex (YYSTYPE *lvalp, YYLTYPE *llocp)
