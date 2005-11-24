%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "parseFuncs.h"
#include "parseul.h"
#include "plog.h"
#define YY_NO_UNPUT
%}

%pointer

FILECHARS ([a-zA-Z0-9_/]|"."|"-")*

%option noyywrap
%option nostdinit

%%
{FILECHARS}".ul"	lvalp->t.ptr = yytext; lvalp->t.len = yyleng; return ULFILE;
{FILECHARS}".uo"	lvalp->t.ptr = yytext; lvalp->t.len = yyleng; return UOFILE;
[a-zA-Z0-9/]*"/"	lvalp->t.ptr = yytext; lvalp->t.len = yyleng; return LOC;
{FILECHARS}".sml" lvalp->t.ptr = yytext; lvalp->t.len = yyleng; return SMLFILE;
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
  }
  else
  {
    oldState = YY_CURRENT_BUFFER;
    newState = yy_create_buffer(file, YY_BUF_SIZE);
    yy_switch_to_buffer(newState);
  }
  i = yyparse(ctx);
//  printf("yyparse returned: %d\n", i);
  if (!top)
  {
    yy_switch_to_buffer(oldState);
    yy_delete_buffer(newState);
  }
  if (i == 0) return Parse_OK;
  if (i == 1) return Parse_ERROR;
  if (i == 2) return Parse_ALLOCERROR;
  return Parse_INTERMALERROR;
}/*}}}*/
