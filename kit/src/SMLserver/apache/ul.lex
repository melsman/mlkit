%{

#include "ul.tab.gra.h"
#include "string.h"

#define YY_DECL int yylex (YYSTYPE *lvalp, YYLTYPE *llocp)

%}
%pointer

ws [\ \t\r\n];
filechars [a-zA-Z0-9/\._-];

%option noyywrap

%%

{ws}+ 
{filechars}+".ul" lvalp->string = strndup(yytext, yyleng); return ULFILE;
{filechars}*".sml" lvalp->string = strndup(yytext, yyleng); return SMLFILE;
{filechars}*".uo" lvalp->string = strndup(yytext, yyleng); return UOFILE;
[a-zA-Z0-9/_-]*"/" lvalp->string = strndup(yytext, yyleng); return LOC;
"As" return AS;
"End" return END;
"Ulfiles" return ULFILES;
"Codefiles" return CODEFILES;
"Scripts" return SCRIPTS;
