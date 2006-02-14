
%%

alphanum=[A-Za-z'_0-9];
id=[A-Za-z]{alphanum};
pathvar="$("([A-Z_]+")";
filebase=({pathvar}|[-A-Za-z_0-9])+;
fileext=({pathvar}|[-A-Za-z_0-9])+;
filename={filebase}("."{fileext})*;
arc=({pathvar}|{filename}|"."|"..");
relpath=({arc}"/")*; 
abspath="/"{relpath};
path={relpath}|{abspath};
file={path}{filename};

ws=("\012"|[\t\ ])*;

%%

<INITIAL>{ws}       => (continue ());
<INITIAL>";"        => (tok (Tokens.SEMICOLON, source, yypos, yypos + 1));
<INITIAL>"="        => (tok (Tokens.EQUALOP, source, yypos, yypos + 1));
<INITIAL>"ann"      => (tok (Tokens.ANN, source, yypos, yypos + 3));
<INITIAL>"and"      => (tok (Tokens.AND, source, yypos, yypos + 3));
<INITIAL>"bas"      => (tok (Tokens.BAS, source, yypos, yypos + 3));
<INITIAL>"basis"    => (tok (Tokens.BASIS, source, yypos, yypos + 5));
<INITIAL>"end"      => (tok (Tokens.END, source, yypos, yypos + 3));
<INITIAL>"in"       => (tok (Tokens.IN, source, yypos, yypos + 2));
<INITIAL>"let"      => (tok (Tokens.LET, source, yypos, yypos + 3));
<INITIAL>"local"    => (tok (Tokens.LOCAL, source, yypos, yypos + 5));
<INITIAL>"open"     => (tok (Tokens.OPEN, source, yypos, yypos + 4));
<INITIAL>{id}       => (tok' (Tokens.ID, yytext, source, yypos));
<INITIAL>{file}     => (tok' (Tokens.FILE, yytext, source, yypos));
<INITIAL>{path}     => (tok' (Tokens.PATH, yytext, source, yypos));

