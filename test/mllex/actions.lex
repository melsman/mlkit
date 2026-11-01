type lexresult = string
type arg = {tag:string,hits:int ref}
val depth = ref 0
val buffer = ref ([]:string list)
fun eof (arg:arg) = "EOF:" ^ #tag arg ^ ":" ^ Int.toString(!(#hits arg))
fun token (kind,text,pos,line,arg:arg) =
  (#hits arg := !(#hits arg)+1;
   kind ^ ":" ^ text ^ ":" ^ Int.toString pos ^ ":" ^ Int.toString line ^ ":" ^ #tag arg)
%%
%structure TestLex
%arg (arg:UserDeclarations.arg);
%posarg
%reject
%count
%full
%s C S;
%%
<INITIAL>"(*" => (depth := 1; YYBEGIN C; continue());
<C>"(*" => (depth := !depth+1; continue());
<C>"*)" => (depth := !depth-1; if !depth = 0 then YYBEGIN INITIAL else (); continue());
<C>. => (continue());
<C>\n => (continue());
<INITIAL>"abc" => (REJECT());
<INITIAL>[a-z]+ => (token("ID",yytext,yypos,!yylineno,arg));
<INITIAL>[0-9]+ => (REJECT());
<INITIAL>[0-9] => (token("DIGIT",yytext,yypos,!yylineno,arg));
<INITIAL>"~" => ("AFTER:" ^ continue());
<INITIAL>"!" => (let val k = continue in k() end);
<INITIAL>^"B" => (token("BOL",yytext,yypos,!yylineno,arg));
<INITIAL>"B" => (token("B",yytext,yypos,!yylineno,arg));
<INITIAL>"\"" => (buffer := []; YYBEGIN S; continue());
<S>"\"" => (YYBEGIN INITIAL; token("STRING",String.concat(List.rev(!buffer)),yypos,!yylineno,arg));
<S>\\n => (buffer := "\n" :: !buffer; continue());
<S>\\\" => (buffer := "\"" :: !buffer; continue());
<S>\\\\ => (buffer := "\\" :: !buffer; continue());
<S>[^"\\\n]+ => (buffer := yytext :: !buffer; continue());
<S>\n => (raise Fail "newline in string");
<INITIAL>\255 => (token("HIGH",yytext,yypos,!yylineno,arg));
<INITIAL>[\ \t\n]+ => (continue());
<INITIAL>"?" => (raise Fail "action failure");
<INITIAL>. => (token("CHAR",yytext,yypos,!yylineno,arg));
