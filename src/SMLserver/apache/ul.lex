%{

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "parseFuncs.h"
#include "parseul.h"

#define YY_NO_UNPUT

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

%%

int
recurseParse(struct parseCtx *ctx, char *filename)/*{{{*/
{
  YY_BUFFER_STATE newState, oldState;
  FILE *file;
  int i, top;
  top = 0;
  if (!ctx->uoTable)
  {
    top = 1;
    ctx->uoTable = (hashtable *) malloc (sizeof(hashtable) * 3);
    if (!ctx->uoTable) return Parse_ALLOCERROR;
    ctx->smlTable = ctx->uoTable+1;
    ctx->ulTable = ctx->uoTable+2;
    if (hashinit(ctx->uoTable, uoHashEntry_HashFun, uoHashEntry_EqualFun) != hash_OK)
      return Parse_ALLOCERROR;
    if (hashinit(ctx->smlTable, char_charHashFun, char_charEqualFun) != hash_OK);
      return Parse_ALLOCERROR;
    if (hashinit(ctx->ulTable, char_charHashFun, char_charEqualFun) != hash_OK);
      return Parse_ALLOCERROR;
  }
  file = fopen (filename, "r");
  if (!file) return Parse_FILEDOESNOTEXISTS;
  if (top) 
  {
    yyin = file;
  }
  else
  {
    oldState = YY_CURRENT_BUFFER;
    newState = yy_create_buffer(file, YY_BUF_SIZE);
    yy_switch_to_buffer(newState);
  }
  i = yyparse(ctx);
  if (top)
  {
    yy_switch_to_buffer(oldState);
    yy_delete_buffer(newState);
  }
  if (i == 0) return Parse_OK;
  if (i == 1) return Parse_ERROR;
  if (i == 2) return Parse_ALLOCERROR;
  return Parse_INTERMALERROR;
}/*}}}*/
