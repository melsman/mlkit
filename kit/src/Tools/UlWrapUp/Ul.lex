  structure Tokens = Tokens
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token
  type lexresult = (svalue,pos) token

  val pos = ref 0
  val eof = fn () => Tokens.EOF(!pos,!pos)


%%
%header (functor UlLexFun(structure Tokens: UL_TOKENS));

ws=[\ \t\r];
filechars=([a-zA-Z0-9_/]|"."|"-")*;

%%

{filechars}".ul"  => (Tokens.ULFILE (yytext,!pos,!pos));
{filechars}".uo"  => (Tokens.UOFILE (yytext,!pos,!pos));
[a-zA-Z0-9/]*"/"  => (Tokens.LOC (yytext,!pos,!pos));
{filechars}".sml" => (Tokens.SML (yytext,!pos,!pos));
"As"              => (Tokens.AS(!pos,!pos));
"End"             => (Tokens.END(!pos,!pos));
"Ulfiles"         => (Tokens.ULFILES(!pos,!pos));
"Codefiles"       => (Tokens.CODEFILES(!pos,!pos));
"Scripts"         => (Tokens.SCRIPTS(!pos,!pos));
ws                => (lex());
"\n"              => (pos:= (!pos) + 1; lex());
EOF               => (Tokens.EOF (!pos,!pos));

