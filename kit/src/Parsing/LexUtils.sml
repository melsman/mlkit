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
	  | glue(".", nil) = Crash.impossible "asQualId.glue"
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

    fun asDigit text = ord text - ord "0"

    local
      fun accumInt(sign, n, xs) =
	case xs
	  of "~" :: xs' => accumInt(~1, n,xs')
	   | "." :: _ => (sign , n, xs)
	   | "E" :: _ => (sign , n, xs)
	   | x :: xs' => accumInt( sign, n * 10 + asDigit x, xs')
	   | nil => (sign , n , nil)

      fun accumDec(mul, n, xs) =
	case xs
	  of "E" :: _ => (n, xs)
	   | x :: xs' => accumDec(mul/10.0, n + mul * real(asDigit x), xs')
	   | nil => (n, nil)

      fun integer_from_list (chars: string list) = 
        let val (sign, n, _) = accumInt(1, 0,chars)
        in sign * n
        end

      fun accumReal(sign, r, xs) =
	case xs
	  of "~" :: xs' => accumReal(~1, r, xs')
	   | "." :: _ => (sign , r, xs)
	   | "E" :: _ => (sign , r, xs)
	   | x :: xs' => accumReal( sign, r * 10.0 + real(asDigit x), xs')
	   | nil => (sign, r, nil)
    in
      fun asInteger text = integer_from_list (explode text)

      (* 31/10/1995-Martin: new; the old couldn't handle 2147483647.0 *)
      fun asReal text =
	let

(*debug
	  val _ = print ("real: " ^  text ^ "\n") 
debug*)

          val ln10 = ln(10.0)
	  val (sign, intPart_as_real, rest) = accumReal(1, 0.0, explode text)

(*debug
	  val _ = BasicIO.println("[intPart=" ^ Real.string intPart_as_real ^ "]")
	  val _ = BasicIO.println("[rest=" ^ implode rest ^ "]")
debug*)

	  val (decPart, rest') =
	    case rest
	      of "." :: xs => accumDec(0.1, 0.0,xs)
	       | _ => (0.0, rest)

(*debug
	  val _ = BasicIO.println("[decPart=" ^ Real.string decPart ^ "]")
	  val _ = BasicIO.println("[rest'=" ^ implode rest' ^ "]")
debug*)

	  val expPart: int =  (* the exponent part, if present, orelse 0 *)
          case rest' of 
               "E" :: xs => integer_from_list xs
	     | _ => 0

(*debug
	  val _ = BasicIO.println("[expPart=" ^ Int.string expPart ^ "]")
debug*)

	  fun E(x, y) = x * exp(y*ln10)
	in
          real(sign)*E(intPart_as_real + decPart, real expPart)
	end handle _ => Crash.impossible("cannot make real constant out of string:" ^ text)

(*old
      fun asReal text =
	let
	  val _ = print ("real: " ^  text ^ "\n") 
          val ln10 = ln(10.0)
	  val (sign, intPart, rest) = accumInt(1, 0, explode text)

	  val _ = BasicIO.println("[intPart=" ^ Int.string intPart ^ "]")
	  val _ = BasicIO.println("[rest=" ^ implode rest ^ "]")


	  val (decPart, rest') =
	    case rest
	      of "." :: xs => accumDec(0.1, 0.0,xs)
	       | _ => (0.0, rest)


	  val _ = BasicIO.println("[decPart=" ^ Real.string decPart ^ "]")
	  val _ = BasicIO.println("[rest'=" ^ implode rest' ^ "]")


	  val expPart: int =  (* the exponent part, if present, orelse 0 *)
          case rest' of 
               "E" :: xs => integer_from_list xs
	     | _ => 0


	  val _ = BasicIO.println("[expPart=" ^ Int.string expPart ^ "]")


	  fun E(x, y) = x * exp(y*ln10)
	in
          real(sign)*E(real intPart + decPart, real expPart)
	end handle _ => Crash.impossible("cannot make real constant out of string:" ^ text)
old*)
    end

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

    fun addAsciiChar (pos, text) arg =
      let
	val ascii =
	  case explode text
	    of ["\\", c1, c2, c3] =>
		 asDigit c1 * 100 + asDigit c2 * 10 + asDigit c3
	     | _ =>
		 Crash.impossible "addAsciiChar"
      in
	if ascii > 255 then
	  raise LexBasics.LEXICAL_ERROR(pos, "bad ASCII escape: " ^ text)
	else
	  addChars (chr ascii) arg
      end

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
