(* Lexical specification for Standard ML.		NICK, August 1990. *)

  open Tokens
  type pos = LexBasics.pos
  type arg = LexUtils.LexArgument
  type lexresult = (svalue, LexBasics.pos) token
  fun eof _ = Tokens.EOF(LexBasics.DUMMY, LexBasics.DUMMY)
  val lParen = "(" and rParen = ")"

(* Something which returns a (pos * pos) for a token. This is what ML-Yacc
   works with, and we use it in the productions below. *)

  fun ofLength(arg, yypos, yytext) =
    let
      val yypos = yypos - 2
	(* If somebody can tell be why the yypos seems to be permanently
	   two characters ahead of where it should be, I'd be interested... *)

      val LexBasics.SOURCE_READER{positionFn, ...} =
	LexUtils.sourceReaderOf arg

      val lPos = positionFn yypos
      val rPos = positionFn(yypos + size yytext)
    in
      (lPos, rPos)
    end

  fun token0(tokFn, arg, yypos, yytext) =
        tokFn(ofLength(arg, yypos, yytext))

  and token1(tokFn, value, arg, yypos, yytext) =
	let
	  val (l, r) = ofLength(arg, yypos, yytext)
	in
	  tokFn(value, l, r)
	end

  fun positionOfStream(arg, yypos) =
    let
      val LexBasics.SOURCE_READER{positionFn, ...} =
	LexUtils.sourceReaderOf arg
    in
      positionFn yypos
    end

  fun error(arg, yypos, msg) =
    raise LexBasics.LEXICAL_ERROR(positionOfStream(arg, yypos), msg)

 (* addAsciiChar can fail, so we need to generate position info for it. *)
  fun addAsciiChar(arg, yypos, yytext) =
    LexUtils.addAsciiChar (positionOfStream(arg, yypos), yytext) arg
%%

%header	(functor TopdecLex(structure Tokens: Topdec_TOKENS
			   structure LexBasics: LEX_BASICS
			   structure LexUtils: LEX_UTILS
			     sharing type LexUtils.svalue = Tokens.svalue
				 and type LexUtils.token = Tokens.token
				 and type LexUtils.pos = LexBasics.pos
				 and type LexUtils.SourceReader
					  = LexBasics.SourceReader
			  )
	);
%arg	(arg: UserDeclarations.arg);

WhiteSpace	   = [\ \t]+;
VWhiteSpace	   = [\ \t\n\012]+;
UC		   = [A-Z];
LC		   = [a-z];
Letter		   = {UC} | {LC};
Digit		   = [0-9];
PosInt		   = {Digit}+;
NegInt		   = \126 {PosInt};
Integer		   = {PosInt} | {NegInt};
Real		   = ({Integer} "." {PosInt} ("E" {Integer})?)
		     | ({Integer} "E" {Integer});
NormalId	   = {Letter} ({Letter} | {Digit} | [_'])*;
TyVar		   = "'" ({Letter} | {Digit} | [_'])*;
Symbol		   = [-!%&$#+<=>?@\\~`^|*:/];
SymbolicId	   = {Symbol}+;
AnyId		   = {NormalId} | {SymbolicId};
QualifiedId	   = ({AnyId} ".")+ {AnyId};
%s	S C;
%%
<INITIAL>{VWhiteSpace}	=> (continue());
<INITIAL>{NormalId}	=> (token1(LexUtils.identifier, yytext,
				   arg, yypos, yytext
				  )
			   );
<INITIAL>{SymbolicId}	=> (token1(LexUtils.identifier, yytext,
				   arg, yypos, yytext
				  )
			   );
<INITIAL>{QualifiedId}	=> (token1(if LexUtils.isQualStar yytext
				   then (LexBasics.shifting "QUAL_STAR";
					 QUAL_STAR
					)
				   else (LexBasics.shifting "QUAL_ID";
					 QUAL_ID
					),
				   LexUtils.asQualId yytext,
				   arg, yypos, yytext
				  )
			   );
<INITIAL>"..."		=> (token0(DOTDOTDOT, arg, yypos, yytext));
<INITIAL>"("		=> (token0(LPAREN, arg, yypos, yytext));
<INITIAL>")"		=> (token0(RPAREN, arg, yypos, yytext));
<INITIAL>"["		=> (token0(LBRACKET, arg, yypos, yytext));
<INITIAL>"]"		=> (token0(RBRACKET, arg, yypos, yytext));
<INITIAL>"{"		=> (token0(LBRACE, arg, yypos, yytext));
<INITIAL>"}"		=> (token0(RBRACE, arg, yypos, yytext));
<INITIAL>","		=> (token0(COMMA, arg, yypos, yytext));
<INITIAL>";"		=> (token0(SEMICOLON, arg, yypos, yytext));
<INITIAL>"_"		=> (token0(UNDERBAR, arg, yypos, yytext));
<INITIAL>{Real}		=> (LexBasics.shifting "REAL(...)";
			    token1(REAL, LexUtils.asReal yytext,
				   arg, yypos, yytext
				  )
			   );
<INITIAL>{Digit}	=> (LexBasics.shifting "DIGIT(...)";
			    token1(DIGIT, LexUtils.asDigit yytext,
				   arg, yypos, yytext
				  )
			   );
<INITIAL>{PosInt}	=> (LexBasics.shifting "POSINT2(...)";
			    token1(POSINT2, LexUtils.asInteger yytext,
				   arg, yypos, yytext
				  )
			   );
<INITIAL>{NegInt}	=> (LexBasics.shifting "NEGINT(...)";
			    token1(NEGINT, LexUtils.asInteger yytext,
				   arg, yypos, yytext
				  )
			   );
<INITIAL>{TyVar}	=> (LexBasics.shifting "TYVAR(...)";
			    token1(TYVAR, yytext, arg, yypos, yytext)
			   );
<INITIAL>\"		=> (YYBEGIN S; lex (LexUtils.clearString arg) ());
<INITIAL>"(*"		=> (YYBEGIN C; lex (LexUtils.newComment arg) ());

<S>[^"\\\n]*		=> (lex (LexUtils.addChars yytext arg) ());
<S>\"			=> (YYBEGIN INITIAL;
			    LexBasics.shifting "STRING(...)";
			    token1(STRING, LexUtils.asString arg,
				   arg, yypos, yytext
				  )
			   );
<S>\n			=> (error(arg, yypos, "unclosed string");
			    YYBEGIN INITIAL;
			    LexBasics.shifting "STRING(bad)";
			    token1(STRING, "", arg, yypos, yytext)
			   );
<S>\\{VWhiteSpace}\\	=> (continue());
<S>\\t			=> (lex (LexUtils.addChars "\t" arg) ());
<S>\\n			=> (lex (LexUtils.addChars "\n" arg) ());
<S>\\\"			=> (lex (LexUtils.addChars "\"" arg) ());
<S>\\\\			=> (lex (LexUtils.addChars "\\" arg) ());
<S>\\\^[@-_]		=> (lex (LexUtils.addControlChar yytext arg) ());
<S>\\[0-9]{3}		=> (lex (addAsciiChar(arg, yypos, yytext)) ());
<S>\\			=> (error(arg, yypos, "illegal string escape");
			    continue()
			   );

<C>"(*"			=> (lex (LexUtils.incComment arg) ());
<C>"*)"			=> (case LexUtils.decComment arg
			      of (0, arg') => (YYBEGIN INITIAL; lex arg' ())
			       | (_, arg') => lex arg' ()
			    );
<C>.			=> (continue());
<C>\n			=> (continue());

.			=> (error(arg, yypos, "cannot lex \"" ^ yytext ^ "\"");
			    continue()
			   );
