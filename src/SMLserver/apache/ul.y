%{
#include <stdlib.h>
#include <stdio.h>
#include <alloca.h>
#include "parseFuncs.h"
#include "parseul.h"
//#define YYDEBUG 1

%}

%pure-parser

%token <t> ULFILE UOFILE LOC SMLFILE GARBAGE

%token <p> ULFILES END CODEFILES SCRIPTS AS 

//Fil UlIncludeList SmlIncludeList UoIncludeList SmlOption

%start Fil

%locations

%error-verbose

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
   /* empty */ {$$ = NULL;}
 | Fil ULFILES UlIncludeList END {$$ = NULL;} 
 | Fil CODEFILES UoIncludeList END {$$ = NULL;} 
 | Fil SCRIPTS SmlIncludeList END {$$ = NULL;}
;
UlIncludeList:
   /* empty */ {$$ = NULL;}
 | UlIncludeList ULFILE SCRIPTS AS LOC { if (toUlHashTable (ctx,$2.ptr, $2.len, $5.ptr, $5.len) != Parse_OK) YYABORT;
                                         $$=NULL; free($2.ptr); free($5.ptr); }
;
UoIncludeList: 
   /* empty */ {$$ = NULL;}
 | UoIncludeList UOFILE { if (extendInterp(ctx, $2.ptr, $2.len) != Parse_OK) YYABORT;
                          $$=NULL; free($2.ptr);}
;
SmlIncludeList:
   /* empty */ {$$ = NULL;}
 | SmlIncludeList UOFILE AS SMLFILE { if (toSmlHashTable(ctx,$2.ptr, $2.len, $4.ptr, $4.len) != Parse_OK) YYABORT;
                                      $$=NULL; free($2.ptr); free($4.ptr);}
 | SmlIncludeList UOFILE { if (toSmlHashTable(ctx,$2.ptr,$2.len,NULL, 0) != Parse_OK) YYABORT;
                           $$=NULL; free($2.ptr);}
;
