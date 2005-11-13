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

Fil:
  /* empty */ {}
  | ULFILES UlIncludeList END Fil {} 
  | CODEFILES UoIncludeList END Fil {} 
  | SCRIPTS SmlIncludeList END Fil {}
;
UlIncludeList:
   /* empty */ { }
 | ULFILE SCRIPTS AS LOC UlIncludeList { toUlHashTable (ctx, $1.ptr,$1.len,$4.ptr,$4.len, fileprefix, fpl) }
;
UoIncludeList:
   /* empty */ { }
 | UOFILE UoIncludeList { extendInterp(ctx, $1.ptr, $1.len, fileprefix, fpl) }
;
SmlIncludeList:
   /* empty */ { }
 | UOFILE SmlOption SmlIncludeList { toSmlHashTable(ctx, $1.ptr,$1.len,$2.ptr,$2.len,prefix, fileprefix, fpl); }
;
SmlOption:
   /* empty */ { s->ptr = NULL; s->len = 0; $$ = *s; }
 | AS UOFILE { $$ = $2 }
;

