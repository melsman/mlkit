%{
#include <stdlib.h>
#include <stdio.h>
#include <alloca.h>
#include "parseFuncs.h"
#define YYDEBUG 1

%}

%pure-parser

%token ULFILE UOFILE LOC

%token ULFILES END CODEFILES SCRIPTS AS 

//Fil UlIncludeList SmlIncludeList UoIncludeList SmlOption



%start Fil

%locations

%parse-param {void *ctx}

%verbose

//%debug

%%

// By using left recursion the semantics actions are performed top down in the file

Fil:
  /* empty */ {$$ = NULL}
  | Fil ULFILES UlIncludeList END { $$ = NULL} 
  | Fil CODEFILES UoIncludeList END {$$ = NULL} 
  | Fil SCRIPTS SmlIncludeList END {$$ = NULL}
;
UlIncludeList:
   /* empty */ { $$ = NULL}
 | UlIncludeList ULFILE SCRIPTS AS LOC { toUlHashTable (ctx,$2,$5); $$=NULL; }
;
UoIncludeList: 
   /* empty */ { $$ = NULL}
 | UoIncludeList UOFILE { extendInterp(ctx, $2); $$=NULL;}
;
SmlIncludeList:
   /* empty */ { $$ = NULL}
 | SmlIncludeList UOFILE AS UOFILE { toSmlHashTable(ctx,$2,$4); $$=NULL; }
 | SmlIncludeList UOFILE { toSmlHashTable(ctx,$2,NULL); $$=NULL; }
;
