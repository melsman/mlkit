%{
#include "parsertree.h"
%}

%pure-parser

%union 
{
  struct parsertree *pt;
  struct smlList *sml;
  struct uoList *uo;
  struct ulList *ul;
  char *string;
}

%token <string> ULFILE UOFILE LOC

%token <intval> ULFILES END CODEFILES SCRIPTS AS 

%type <sml> SmlIncludeList 
%type <uo> UoIncludeList 
%type <ul> UlIncludeList 
%type <string> SmlOption
%type <pt> Fil

%start Fil

%locations

%parse-param {parsertree *pt}

%verbose

%%

Fil:
  /* empty */ { $$ = NULL; }
  | ULFILES UlIncludeList END Fil {} 
  | CODEFILES UoIncludeList END Fil {} 
  | SCRIPTS SmlIncludeList END Fil {}
;
UlIncludeList:
   /* empty */ { $$ = NULL; }
 | ULFILE SCRIPTS AS LOC UlIncludeList {}
;
UoIncludeList:
   /* empty */ { $$ = NULL; }
 | UOFILE UoIncludeList {}
;
SmlIncludeList:
   /* empty */ { $$ = NULL; }
 | UOFILE SmlOption SmlIncludeList {}
;
SmlOption:
   /* empty */ { $$ = NULL; }
 | AS UOFILE { $$ = $2; }
;
