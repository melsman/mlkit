(* Lexical specification for Standard ML.		NICK, August 1990. *)

  open Tokens
  type pos = LexBasics.pos
  type arg = LexUtils.LexArgument
  type lexresult = (svalue, LexBasics.pos) token
  fun eof _ = Tokens.EOF(LexBasics.DUMMY, LexBasics.DUMMY)
  val lParen = "(" and rParen = ")"

  fun pad str = if size str mod 18 <> 0 then pad(str ^ " ") else str
  fun shifting x = () (*BasicIO.print(pad("S." ^ x ^ " "))*)

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

  fun token1(tokFn, value, arg, yypos, yytext) =
	let
	  val (l, r) = ofLength(arg, yypos, yytext)
	in
	  tokFn(value, l, r)
	end

  fun token_id (yytext, arg, yypos) =
    token1(LexUtils.identifier, yytext,
	   arg, yypos, yytext)

  fun token_qualid (yytext,arg,yypos) =
    token1(if LexUtils.isQualStar yytext
	     then (shifting "QUAL_STAR";
		   QUAL_STAR
		   )
	   else (shifting "QUAL_ID";
		 QUAL_ID
		 ),
	     LexUtils.asQualId yytext,
	     arg, yypos, yytext
	     )

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

  fun has_quote s = 
    let fun loop i = ((String.sub(s,i) = #"`") orelse loop (i+1))
      handle _ => false
    in loop 0
    end

  fun incr r = r := !r + 1
  fun decr r = r := !r - 1

  val quotation = Flags.is_on0 "quotation"
%%

%header	(functor TopdecLex(structure Tokens: Topdec_TOKENS
			   structure LexBasics: LEX_BASICS
			   structure LexUtils: LEX_UTILS
			     sharing type LexUtils.svalue = Tokens.svalue
			     sharing type LexUtils.token = Tokens.token
			     sharing type LexUtils.pos = LexBasics.pos
			     sharing type LexUtils.SourceReader
					  = LexBasics.SourceReader
                           structure Flags : FLAGS
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
NormalIdOrExport   = "_export" | {NormalId};
TyVar		   = "'" ({Letter} | {Digit} | [_'])*;
Symbol		   = [-!%&$#+<=>?@\\~`^|*:/];
SymbolicId	   = {Symbol}+;
AnyId		   = {NormalId} | {SymbolicId};
QualifiedId	   = ({AnyId} ".")+ {AnyId};

SymbolNoQuote      = [-!%&$#+<=>?@\\~^|*:/];
SymbolicIdNoQuote  = {SymbolNoQuote}+;
AnyIdNoQuote       = {NormalId} | {SymbolicIdNoQuote};
QualifiedIdNoQuote = ({AnyIdNoQuote} ".")+ {AnyIdNoQuote};

%reject
%s	S C Q AQ;
%%
<INITIAL>{VWhiteSpace}	=> (continue());
<INITIAL>{NormalIdOrExport} => (token_id(yytext,arg,yypos));
<INITIAL>"..."		=> (token0(DOTDOTDOT, arg, yypos, yytext));
<INITIAL>"("		=> ((if LexUtils.parStackIsEmpty arg then ()
			     else incr (LexUtils.parStackTop arg));
			    token0(LPAREN, arg, yypos, yytext)
			   );
<INITIAL>")"		=> ((if LexUtils.parStackIsEmpty arg then ()
			     else let val top = LexUtils.parStackTop arg
				  in if !top = 1 then (LexUtils.parStackPop arg;
						       LexUtils.clearString arg;
						       YYBEGIN Q)
				     else decr top
				  end);
			    token0(RPAREN, arg, yypos, yytext)
			   );
<INITIAL>"["		=> (token0(LBRACKET, arg, yypos, yytext));
<INITIAL>"]"		=> (token0(RBRACKET, arg, yypos, yytext));
<INITIAL>"{"		=> (token0(LBRACE, arg, yypos, yytext));
<INITIAL>"}"		=> (token0(RBRACE, arg, yypos, yytext));
<INITIAL>","		=> (token0(COMMA, arg, yypos, yytext));
<INITIAL>";"		=> (token0(SEMICOLON, arg, yypos, yytext));
<INITIAL>"_"		=> (token0(UNDERBAR, arg, yypos, yytext));
<INITIAL>{Real}		=> (shifting "REAL(...)";
			    token1(REAL, LexUtils.asReal yytext,
				   arg, yypos, yytext));
<INITIAL>{Digit}	=> (shifting "DIGIT(...)";
			    token1(DIGIT, LexUtils.asDigit yytext,
				   arg, yypos, yytext));
<INITIAL>{DecPosInteger}=> (shifting "DECPOSINTEGER(...)";
			    token1(DECPOSINTEGER, LexUtils.asInteger yytext,
				   arg, yypos, yytext));
<INITIAL>{DecNegInteger}=> (shifting "DECNEGINTEGER(...)";
			    token1(DECNEGINTEGER, LexUtils.asInteger yytext,
				   arg, yypos, yytext));
<INITIAL>{HexInteger}   => (shifting "HEXINTEGER(...)";
			    token1(HEXINTEGER, LexUtils.asInteger yytext,
				   arg, yypos, yytext));
<INITIAL>{Word}         => (shifting "WORD(...)";
			    token1(WORD, LexUtils.asWord yytext,
				   arg, yypos, yytext));
<INITIAL>{TyVar}	=> (shifting "TYVAR(...)";
			    token1(TYVAR, yytext, arg, yypos, yytext));
<INITIAL>"\""		=> (YYBEGIN S; LexUtils.clearString arg; continue());
<INITIAL>"(*"		=> (YYBEGIN C; LexUtils.newComment arg; continue());

<INITIAL>{SymbolicId}	=> (if quotation() andalso has_quote yytext then REJECT()
                            else token_id(yytext,arg,yypos));
<INITIAL>{QualifiedId}	=> (if quotation() andalso has_quote yytext then REJECT()
                            else token_qualid(yytext,arg,yypos));
<INITIAL>{SymbolicIdNoQuote} => (token_id(yytext,arg,yypos));
<INITIAL>{QualifiedIdNoQuote} => (token_qualid(yytext,arg,yypos));

<INITIAL>"`"            => ((* a starting quote *)
                            YYBEGIN Q; 
                            LexUtils.clearString arg;
                            token0 (BEGINQ,arg,yypos,yytext));

<Q>"^`"                 => (LexUtils.addChars "`" arg; continue());
<Q>"^^"                 => (LexUtils.addChars "^" arg; continue());
<Q>"^"                  => (YYBEGIN AQ;
			    token1(OBJL, LexUtils.asString arg,
	                           arg, yypos, yytext)
			    );
<Q>"`"                  => ((* a closing quote *)
                            YYBEGIN INITIAL;
			    token1(ENDQ, LexUtils.asString arg,
	                           arg, yypos, yytext)
			    );
<Q>.                    => (LexUtils.addChars yytext arg; continue());
<Q>\n                   => (LexUtils.addChars yytext arg; continue());

<AQ>{VWhiteSpace}       => (continue());
<AQ>{NormalId}          => (YYBEGIN Q; LexUtils.clearString arg; 
			    token1(AQID, yytext, arg, yypos, yytext));
<AQ>"("                 => (YYBEGIN INITIAL;
			    LexUtils.parStackPush (ref 1) arg;
			    token0(LPAREN, arg, yypos, yytext)
			   );
<AQ>.                   => (error(arg, yypos, "bad character after antiquote");
			    continue());

<S>[^"\\\n]*		=> (LexUtils.addChars yytext arg; continue());
<S>"\""			=> (YYBEGIN INITIAL;
			    shifting "STRING(...)";
			    token1(STRING, LexUtils.asString arg,
				   arg, yypos, yytext)
			   );
<S>\n			=> (error(arg, yypos, "unclosed string");
			    YYBEGIN INITIAL;
			    shifting "STRING(bad)";
			    token1(STRING, "", arg, yypos, yytext)
			   );
<S>\\{VWhiteSpace}\\	=> (continue());
<S>\\a			=> (LexUtils.addChars (str(chr 7)) arg; continue());
<S>\\b			=> (LexUtils.addChars (str(chr 8)) arg; continue());
<S>\\t			=> (LexUtils.addChars "\t" arg; continue());
<S>\\n			=> (LexUtils.addChars "\n" arg; continue());
<S>\\v			=> (LexUtils.addChars (str(chr 11)) arg; continue());
<S>\\f			=> (LexUtils.addChars (str(chr 12)) arg; continue());
<S>\\r			=> (LexUtils.addChars (str(chr 13)) arg; continue());
<S>\\\^[@-_]		=> (LexUtils.addControlChar yytext arg; continue());
<S>\\[0-9]{3}		=> (addAsciiChar (arg, yypos, yytext); continue());
<S>\\u{HexDigit}{4}	=> (addUnicodeChar (arg, yypos, yytext); continue());
<S>\\\"			=> (LexUtils.addChars "\"" arg; continue());
<S>\\\\			=> (LexUtils.addChars "\\" arg; continue());
<S>\\			=> (error(arg, yypos, "illegal string escape");
			    continue()
			   );

<C>"(*"			=> (LexUtils.incComment arg; continue());
<C>"*)"			=> (case LexUtils.decComment arg
			      of 0 => (YYBEGIN INITIAL; continue())
			       | _ => continue()
			    );
<C>.			=> (continue());
<C>\n			=> (continue());

.			=> (error(arg, yypos, "cannot lex \"" ^ yytext ^ "\"");
			    continue()
			   );
