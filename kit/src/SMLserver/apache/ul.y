%{
#include <stdlib.h>
#include <alloca.h>
#include "parseFuncs.h"
#include "mod_sml.h"

%}

%pure-parser

%token ULFILE UOFILE LOC

%token ULFILES END CODEFILES SCRIPTS AS 

//Fil UlIncludeList SmlIncludeList UoIncludeList SmlOption



%start Fil

%locations

%parse-param {void *ctx}
%parse-param {char *prefix}
%parse-param {char *fileprefix}
%parse-param {int fpl}
%parse-param {struct data *s}

%verbose

%%
// By using left recursion the semantics actions are performed top down in the file
Fil:
  /* empty */ {}
  | Fil ULFILES UlIncludeList END {} 
  | Fil CODEFILES UoIncludeList END {} 
  | Fil SCRIPTS SmlIncludeList END {}
;
UlIncludeList:
   /* empty */ { }
 | UlIncludeList ULFILE SCRIPTS AS LOC { toUlHashTable (ctx, $2.ptr,$2.len,$5.ptr,$5.len) }
;
UoIncludeList: 
   /* empty */ { }
 | UoIncludeList UOFILE { extendInterp(ctx, $2.ptr, $2.len) }
;
SmlIncludeList:
   /* empty */ { }
 | SmlIncludeList UOFILE SmlOption { toSmlHashTable(ctx, $2.ptr,$2.len,$3.ptr,$3.len); }
;
SmlOption:
   /* empty */ { s->ptr = NULL; s->len = 0; $$ = *s; }
 | AS UOFILE { $$ = $2 }
;

