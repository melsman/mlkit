%{

#include "string.h"
#include "parseFuncs.h"

%}
%pointer

ws [\ \t\r\n];
filechars [a-zA-Z0-9/\._-];

%option noyywrap

%%

{ws}+ 
{filechars}*".ul" lvalp->ptr = yytext; lvalp->len = yyleng; return ULFILE;
{filechars}*".uo" lvalp->ptr = yytext; lvalp->len = yyleng; return UOFILE;
[a-zA-Z0-9/_-]*"/" lvalp->ptr = yytext; lvalp->len = yyleng; return LOC;
"As" return AS;
"End" return END;
"Ulfiles" return ULFILES;
"Codefiles" return CODEFILES;
"Scripts" return SCRIPTS;
