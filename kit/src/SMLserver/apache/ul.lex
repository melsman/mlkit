%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "parseFuncs.h"
#include "parseul.h"
#include "plog.h"
#define YY_NO_UNPUT

int
pack(char *in, int size, int kind, YYSTYPE *lv)/*{{{*/
{
  char *tmp;
  tmp = (char *) malloc(size + 1);
  if (!tmp)
  {
    lv->t.ptr = NULL;
    lv->t.len = 0;
    return kind;
  }
  lv->t.ptr = tmp;
  lv->t.len = size;
  strncpy(tmp, in, size);
  tmp[size] = 0;
  return kind;
}/*}}}*/

%}

%pointer

FILECHARS ([a-zA-Z0-9_/]|"."|"-")*

%option noyywrap
%option nostdinit

%%
{FILECHARS}".ul"	return pack(yytext,yyleng,ULFILE, lvalp);
{FILECHARS}".uo"	return pack(yytext,yyleng,UOFILE, lvalp);
[a-zA-Z0-9/]*"/"	return pack(yytext,yyleng,LOC, lvalp);
{FILECHARS}".sml" return pack(yytext,yyleng,SMLFILE, lvalp);
"As"	return AS;
"End"	return END;
"Ulfiles" return ULFILES;
"Codefiles" return CODEFILES;
"Scripts" return SCRIPTS;
[ \t\n]+ 

%%

extern int yydebug;

int
recurseParse(struct parseCtx *ctx, char *filename)/*{{{*/
{
  YY_BUFFER_STATE newState, oldState;
  FILE *file;
  int i, top;
  top = 0;
  plog2s("recurseParse called upon ", filename, ctx->ctx);
//  yydebug = 1;
  if (!ctx->uoTable)
  {
    top = 1;
    ctx->uoTable = (hashtable *) calloc (1, sizeof(hashtable));
    if (!ctx->uoTable) return Parse_ALLOCERROR;
    ctx->ulTable = (hashtable *) calloc (1, sizeof(hashtable));
    if (!ctx->ulTable)
    {
      free(ctx->uoTable);
      return Parse_ALLOCERROR;
    }
    ctx->smlTable = (hashtable *) calloc (1, sizeof(hashtable));
    if (!ctx->smlTable)
    {
      free(ctx->ulTable);
      free(ctx->uoTable);
      return Parse_ALLOCERROR;
    }
    if (hashinit(ctx->uoTable, uoHashEntry_HashFun, uoHashEntry_EqualFun) != hash_OK)
    {
      free(ctx->smlTable);
      free(ctx->ulTable);
      free(ctx->uoTable);
      return Parse_ALLOCERROR;
    }
    if (hashinit(ctx->smlTable, char_charHashFun, char_charEqualFun) != hash_OK)
    {
      free(ctx->smlTable);
      free(ctx->ulTable);
      free(ctx->uoTable);
      return Parse_ALLOCERROR;
    }
    if (hashinit(ctx->ulTable, char_charHashFun, char_charEqualFun) != hash_OK)
    {
      free(ctx->smlTable);
      free(ctx->ulTable);
      free(ctx->uoTable);
      return Parse_ALLOCERROR;
    }
    plog2s("recurseParse hash tables initialized", "", ctx->ctx);
  }
  file = fopen (filename, "r");
  if (!file) return Parse_FILEDOESNOTEXISTS;
  if (top) 
  {
    yyin = file;
    i = yyparse(ctx);
  }
  else
  {
    oldState = YY_CURRENT_BUFFER;
    newState = yy_create_buffer(file, YY_BUF_SIZE);
    yy_switch_to_buffer(newState);
    i = yyparse(ctx);
    yy_switch_to_buffer(oldState);
    yy_delete_buffer(newState);
  }
//  printf("yyparse returned: %d\n", i);
  if (i == 0) return Parse_OK;
  if (i == 1) return Parse_ERROR;
  if (i == 2) return Parse_ALLOCERROR;
  return Parse_INTERMALERROR;
}/*}}}*/
