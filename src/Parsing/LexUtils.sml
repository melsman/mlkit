(* General lexing utilities. *)

(*$LexUtils: LEX_BASICS ParseSML_ BASIC_IO FLAGS CRASH LEX_UTILS*)
functor LexUtils(structure LexBasics: LEX_BASICS
		 structure Token: Topdec_TOKENS
		 structure BasicIO: BASIC_IO
		 structure Flags: FLAGS
		 structure Crash: CRASH
		): LEX_UTILS =
  struct
    open LexBasics Token
    fun impossible s = Crash.impossible ("LexUtils." ^ s)
    fun noSome None s = impossible s
      | noSome (Some x) s = x

    datatype LexArgument = LEX_ARGUMENT of {sourceReader: SourceReader,
					    stringChars: string list,
					    commentDepth: int
					   }

    fun sourceReaderOf(LEX_ARGUMENT{sourceReader, ...}) = sourceReader

    type arg = LexArgument

    fun asQualId text =
      let
	fun glue(".", y :: ys) = (* "." :: *) glue(y, ys)
	  | glue(x, "." :: ys) = x :: glue(".", ys)
	  | glue(x, y :: ys) = glue(x ^ y, ys)
	  | glue(".", nil) = impossible "asQualId.glue"
	  | glue(x, nil) = [x]
      in
	glue("", explode text)
      end

    val asQualId =
      if !Flags.DEBUG_LEXING then
	fn text =>
	  let
	    val result = asQualId text
	  in
	    BasicIO.println("asQualId(" ^ text ^ ") = "
			    ^ List.stringSep "[" "]" ", " (fn x => x) result
			   );
	    result
	  end
      else asQualId

    fun isQualStar id =
      case List.rev(asQualId id)
	of "*" :: _ => true
	 | _ => false			(* We can't get nil (or [_]). *)

    local
      fun chars_to_int ("~" :: chars) = ~1 * chars_to_posint chars
	| chars_to_int chars = chars_to_posint chars
      and chars_to_posint ("0" :: "x" :: chars) =
	    chars_to_posint_in_base 16 chars
	| chars_to_posint chars = chars_to_posint_in_base 10 chars
      and chars_to_posint_in_base base chars =
	    chars_to_posint_in_base0 base 0 chars
      and chars_to_posint_in_base0 base n [] = n
	| chars_to_posint_in_base0 base n (char :: chars) =
	    (case char_to_int_opt base char of
	       Some i => chars_to_posint_in_base0 base (n * base + i) chars
	     | None => n)
      and char_to_int_opt base char =
	    let val i = if StringType.isUpper char then ord char - ord "A" + 10
			else if StringType.isLower char then ord char - ord "a" + 10
			     else if StringType.isDigit char then ord char - ord "0"
				  else ~1 (*hack*)
	    in 
	      if i>=0 andalso i<base then Some i else None
	    end handle _ => None
      fun char_to_int base char =
	    noSome (char_to_int_opt base char) "char_to_int"

      fun accumDec (mul, n, xs) =
	    (case xs of
	       "E" :: _ => (n, xs)
	     | x :: xs' => accumDec (mul/10.0, n + mul * real (char_to_int 10 x), xs')
	     | nil => (n, nil))

      (*accumReal looks very much like accumInt (now chars_to_int) looked,
       but you must use accumReal, as int's haven't got the sufficient
       precision to convert the int part of a real.*)

      fun accumReal (sign, r, xs) =
	    (case xs of
	       "~" :: xs' => accumReal(~1, r, xs')
	     | "." :: _ => (sign , r, xs)
	     | "E" :: _ => (sign , r, xs)
	     | x :: xs' => accumReal (sign, r * 10.0 + real (char_to_int 10 x), xs')
	     | nil => (sign, r, nil))

      fun asWord0 ("0" :: "w" :: "x" :: chars) =
	    chars_to_posint_in_base 16 chars 
	| asWord0 ("0" :: "w" :: chars) = chars_to_posint_in_base 10 chars
	| asWord0 _ = impossible "asWord0"

      fun exception_to_opt p x = Some (p x) handle Overflow => None
    in
      val asInteger = exception_to_opt (chars_to_int o explode)
      val asWord = exception_to_opt (asWord0 o explode)
      fun chars_to_posint_in_base chars = chars_to_posint_in_base chars
	    handle Overflow => impossible "chars_to_posint_in_base"
      (*31/10/1995-Martin: new; the old couldn't handle 2147483647.0:
       (because it used accumInt instead of accumReal)*)
      fun asReal text =
	let
          val ln10 = ln(10.0)
	  val (sign, intPart_as_real, rest) = accumReal(1, 0.0, explode text)
	  val (decPart, rest') = (case rest of
				    "." :: xs => accumDec (0.1, 0.0, xs)
				  | _ => (0.0, rest))

	  (* the exponent part, if present, orelse 0 *)
	  val expPart : int = (case rest' of 
				 "E" :: xs => chars_to_int xs
			       | _ => 0)
	  fun E(x, y) = x * exp(y*ln10)
	in
          Some (real sign*E(intPart_as_real + decPart, real expPart))
	end handle _ => None
    end (*local*)

    fun initArg sourceReader = LEX_ARGUMENT{sourceReader=sourceReader,
					    stringChars=nil,
					    commentDepth=0
					   }

    fun clearString arg = initArg(sourceReaderOf arg)

    fun newComment arg = LEX_ARGUMENT{sourceReader=sourceReaderOf arg,
				      stringChars=nil, commentDepth=1
				     }

    fun addChars text (LEX_ARGUMENT{sourceReader, stringChars, ...}) =
      LEX_ARGUMENT{sourceReader=sourceReader,
		   stringChars=text :: stringChars, commentDepth=0
		  }

    fun addControlChar text arg =
      addChars (chr(ord(String.nth 1 text) - ord "@")) arg

    fun asDigit text = ord text - ord "0"

    local
      fun add_numbered_char (pos, text) arg limit n =
	    if n > limit then
	      raise LexBasics.LEXICAL_ERROR
		      (pos, "ASCII escape " ^ text ^ " must be <= " ^ Int.string limit)
	    else
	      addChars (chr n) arg
    in
      fun addAsciiChar (pos, text) arg =
	    add_numbered_char (pos, text) arg 255
	      (case explode text of
		 ["\\", c1, c2, c3] =>
		   chars_to_posint_in_base 10 [c1, c2, c3]
	       | _ => impossible "addAsciiChar")

      (*indtil videre, `addUnicodeChar' will only allow `unicode' chars in
       the range [0; 255].  20/06/1997 18:53. tho.*)

      fun addUnicodeChar (pos, text) arg =
	    add_numbered_char (pos, text) arg 255
	      (case explode text of
		 ["\\", "u", c1, c2, c3, c4] =>
		   chars_to_posint_in_base 16 [c1, c2, c3, c4]
	       | _ => impossible "addUnicodeChar")
    end (*local*)

    fun asString(LEX_ARGUMENT{stringChars, ...}) = implode(rev stringChars)

   (*keyword detection (better done here than by the lexer). *)

    fun identifier(text, p1, p2) =
      let
	fun keyword tok = (shifting("KEY(" ^ text ^ ")"); tok(p1, p2))
      in
        case text
	  of "abstype"	 => keyword ABSTYPE
	   | "and"	 => keyword AND
	   | "andalso"	 => keyword ANDALSO
	   | "as"	 => keyword AS
	   | "case"	 => keyword CASE
	   | "do"	 => keyword DO
	   | "datatype"	 => keyword DATATYPE
	   | "else"	 => keyword ELSE
	   | "end"	 => keyword END
	   | "eqtype"	 => keyword EQTYPE
	   | "exception" => keyword EXCEPTION
	   | "fn"	 => keyword FN
	   | "fun"	 => keyword FUN
	   | "functor"	 => keyword FUNCTOR
	   | "handle"	 => keyword HANDLE
	   | "if"	 => keyword IF
	   | "in"	 => keyword IN
	   | "include"	 => keyword INCLUDE
	   | "infix"	 => keyword INFIX
	   | "infixr"	 => keyword INFIXR
	   | "let"	 => keyword LET
	   | "local"	 => keyword LOCAL
	   | "nonfix"	 => keyword NONFIX
	   | "of"	 => keyword OF
	   | "op"	 => keyword OP
	   | "open"	 => keyword OPEN
	   | "orelse"	 => keyword ORELSE
	   | "raise"	 => keyword RAISE
	   | "rec"	 => keyword REC
	   | "sharing"	 => keyword SHARING
	   | "sig"	 => keyword SIG
	   | "signature" => keyword SIGNATURE
	   | "struct"	 => keyword STRUCT
	   | "structure" => keyword STRUCTURE
	   | "then"	 => keyword THEN
	   | "type"	 => keyword TYPE
	   | "val"	 => keyword VAL
	   | "where"	 => keyword WHERE
	   | "with"	 => keyword WITH
	   | "withtype"	 => keyword WITHTYPE
	   | "while"	 => keyword WHILE

	   | ":"	 => keyword COLON
	   | ":>"	 => keyword COLONGREATER
	   | "|"	 => keyword BAR
	   | "="	 => keyword EQUALS
	   | "=>"	 => keyword DARROW
	   | "->"	 => keyword ARROW
	   | "#"	 => keyword HASH
	   | "*"	 => keyword STAR
					(* Not actually reserved, but ... *)

	   | _		 => (shifting("ID(" ^ text ^ ")"); ID(text, p1, p2))
      end

    fun incComment(LEX_ARGUMENT{sourceReader, commentDepth, ...}) =
      LEX_ARGUMENT{sourceReader=sourceReader,
		   stringChars=nil, commentDepth=commentDepth+1
		  }

    fun decComment(LEX_ARGUMENT{sourceReader, commentDepth, ...}) =
      (commentDepth-1,
       LEX_ARGUMENT{sourceReader=sourceReader,
		    stringChars=nil, commentDepth=commentDepth-1
		   }
      )
  end;
