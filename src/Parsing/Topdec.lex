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

 (*addUnicodeChar can fail, so we need to generate position info for it.*)
  fun addUnicodeChar (arg, yypos, yytext) =
        LexUtils.addUnicodeChar (positionOfStream (arg, yypos), yytext) arg
%%

%header	(functor TopdecLex(structure Tokens: Topdec_TOKENS
			   structure LexBasics: LEX_BASICS
			   structure LexUtils: LEX_UTILS
			     sharing type LexUtils.svalue = Tokens.svalue
			     sharing type LexUtils.token = Tokens.token
			     sharing type LexUtils.pos = LexBasics.pos
			     sharing type LexUtils.SourceReader
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
DecPosInteger	   = {Digit}+;
DecNegInteger	   = \126 {DecPosInteger};
HexDigit           = [0-9a-fA-F];
HexInteger         = (\126)? "0x" {HexDigit}+;
Word               = "0w" ("x" {HexDigit}+ | {Digit}+);
DecInteger	   = {DecPosInteger} | {DecNegInteger};
Real		   = ({DecInteger} "." {DecPosInteger} ("E" {DecInteger})?)
		     | ({DecInteger} "E" {DecInteger});
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
				   arg, yypos, yytext));
<INITIAL>{Digit}	=> (LexBasics.shifting "DIGIT(...)";
			    token1(DIGIT, LexUtils.asDigit yytext,
				   arg, yypos, yytext));
<INITIAL>{DecPosInteger}=> (LexBasics.shifting "DECPOSINTEGER(...)";
			    token1(DECPOSINTEGER, LexUtils.asInteger yytext,
				   arg, yypos, yytext));
<INITIAL>{DecNegInteger}=> (LexBasics.shifting "DECNEGINTEGER(...)";
			    token1(DECNEGINTEGER, LexUtils.asInteger yytext,
				   arg, yypos, yytext));
<INITIAL>{HexInteger}   => (LexBasics.shifting "HEXINTEGER(...)";
			    token1(HEXINTEGER, LexUtils.asInteger yytext,
				   arg, yypos, yytext));
<INITIAL>{Word}         => (LexBasics.shifting "WORD(...)";
			    token1(WORD, LexUtils.asWord yytext,
				   arg, yypos, yytext));
<INITIAL>{TyVar}	=> (LexBasics.shifting "TYVAR(...)";
			    token1(TYVAR, yytext, arg, yypos, yytext));
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
<S>\\a			=> (lex (LexUtils.addChars (str(chr 7)) arg) ());
<S>\\b			=> (lex (LexUtils.addChars (str(chr 8)) arg) ());
<S>\\t			=> (lex (LexUtils.addChars "\t" arg) ());
<S>\\n			=> (lex (LexUtils.addChars "\n" arg) ());
<S>\\v			=> (lex (LexUtils.addChars (str(chr 11)) arg) ());
<S>\\f			=> (lex (LexUtils.addChars (str(chr 12)) arg) ());
<S>\\r			=> (lex (LexUtils.addChars (str(chr 13)) arg) ());
<S>\\\^[@-_]		=> (lex (LexUtils.addControlChar yytext arg) ());
<S>\\[0-9]{3}		=> (lex (addAsciiChar (arg, yypos, yytext)) ());
<S>\\u{HexDigit}{4}	=> (lex (addUnicodeChar (arg, yypos, yytext)) ());
<S>\\\"			=> (lex (LexUtils.addChars "\"" arg) ());
<S>\\\\			=> (lex (LexUtils.addChars "\\" arg) ());
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
