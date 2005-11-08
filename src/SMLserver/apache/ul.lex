structure Tokens = Tokens
type pos = int

 type svalue = Tokens.svalue
 type ('a,'b) token = ('a,'b) Tokens.token
 type lexresult = (svalue,pos) token

val eof = fn () => Tokens.EOF

%%

%header (functor UlGrammar(structure Tokens: UlGrammar_TOKENS));

ws=[\ \t\r\n];

%%

{ws}+ => (lex());
[a-zA-Z0-9/\._-]*\.ul => (Tokens.ULFILE yytext);
[a-zA-Z0-9/\._-]*\.sml => (Tokens.SMLFILE yytext);
[a-zA-Z0-9/\._-]*\.uo => (Tokens.UOFILE yytext);
[a-zA-Z0-9/_-]*\/ => (Tokens.LOC yytext);
"As" => (Tokens.AS);
"End" => (Tokens.END);
"Ulfiles" => (Tokens.ULFILES);
"Codefiles" => (Tokens.CODEFILES);
"Scripts" => (Tokens.SCRIPTS);
