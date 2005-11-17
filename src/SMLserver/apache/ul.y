%{
#include <stdlib.h>
#include <stdio.h>
#include <alloca.h>
#include "parseFuncs.h"
#define YYDEBUG 1

%}

%pure-parser

%token <t> ULFILE UOFILE LOC

%token <p> ULFILES END CODEFILES SCRIPTS AS 

//Fil UlIncludeList SmlIncludeList UoIncludeList SmlOption

%start Fil

%locations

%parse-param {void *ctx}

%verbose

/*
%union
{
  struct tokens t;
  void *p;
};*/

//%type <tokens> UOFILE ULFILE LOC
%type <p> Fil UlIncludeList UoIncludeList SmlIncludeList 

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
 | UlIncludeList ULFILE SCRIPTS AS LOC { toUlHashTable (ctx,$2.ptr, $2.len, $5.ptr, $5.len); $$=NULL; }
;
UoIncludeList: 
   /* empty */ { $$ = NULL}
 | UoIncludeList UOFILE { extendInterp(ctx, $2.ptr, $2.len); $$=NULL;}
;
SmlIncludeList:
   /* empty */ { $$ = NULL}
 | SmlIncludeList UOFILE AS UOFILE { toSmlHashTable(ctx,$2.ptr, $2.len, $4.ptr, $4.len); $$=NULL; }
 | SmlIncludeList UOFILE { toSmlHashTable(ctx,$2.ptr,$2.len,NULL, 0); $$=NULL; }
;
