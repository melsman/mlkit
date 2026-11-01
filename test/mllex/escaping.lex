datatype token = Token of string | Resume of unit -> token
type lexresult = token
fun eof () = Token "EOF"
%%
%reject
%structure Escaping
%%
"rej" => (Resume REJECT);
"abc" => (case REJECT() of Token s => Token("REJECT:"^s) | t => t);
[a-z]+ => (Token yytext);
"@" => (Resume continue);
"!" => (case continue() of Token s => Token("AFTER:"^s) | t => t);
[\ \t\n]+ => (continue());
. => (Token yytext);
