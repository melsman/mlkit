(* General lexing utilities. *)

functor LexUtils(structure LexBasics: LEX_BASICS
		 structure Token: Topdec_TOKENS
		 structure BasicIO: BASIC_IO
		 structure Flags: FLAGS
		 structure Crash: CRASH
		): LEX_UTILS =
  struct
    
    open LexBasics Token
    fun impossible s = Crash.impossible ("LexUtils." ^ s)
    fun noSome NONE s = impossible s
      | noSome (SOME x) s = x

    datatype LexArgument = LEX_ARGUMENT of {sourceReader: SourceReader,
					    stringChars: string list,
					    commentDepth: int
					   }

    fun sourceReaderOf(LEX_ARGUMENT{sourceReader, ...}) = sourceReader

    type arg = LexArgument

    fun asQualId text =
      let
	fun glue(".", y :: ys) = (* "." :: *) glue(str y, ys)
	  | glue(x, #"." :: ys) = x :: glue(".", ys)
	  | glue(x, y :: ys) = glue(x ^ str y, ys)
	  | glue(".", nil) = impossible "asQualId.glue"
	  | glue(x, nil) = [x]
      in
	glue("", explode text)
      end

    fun isQualStar id =
      case List.rev(asQualId id)
	of "*" :: _ => true
	 | _ => false			(* We can't get nil (or [_]). *)

    local
      fun ordw c = Word32.fromInt(ord c)

      fun chars_to_w (#"0" :: #"x" :: chars) = chars_to_w_in_base 0w16 chars
	| chars_to_w chars = chars_to_w_in_base 0w10 chars
      and chars_to_w_in_base base chars = chars_to_w_in_base0 base 0w0 chars
      and chars_to_w_in_base0 base n [] = n
	| chars_to_w_in_base0 base n (char :: chars) =
	    (case char_to_w_opt base char of
	       SOME i => (* a new digit is added; manually raise Overflow if 
			  * new value is smaller than old value *)
		 let val new = n * base + i
		 in if new < n then raise Overflow
		    else chars_to_w_in_base0 base new chars
		 end
	     | NONE => n)
      and char_to_w_opt base char =
	let val i = if Char.isUpper char then ordw char - ordw #"A" + 0w10
		    else if Char.isLower char then ordw char - ordw #"a" + 0w10
			 else if Char.isDigit char then ordw char - ordw #"0"
			      else base (*hack*)
	in 
	  if i<base then SOME i else NONE
	end handle _ => NONE

      fun asWord0 (#"0" :: #"w" :: #"x" :: chars) = chars_to_w_in_base 0w16 chars 
	| asWord0 (#"0" :: #"w" :: chars) = chars_to_w_in_base 0w10 chars
	| asWord0 _ = impossible "asWord0"

      fun exception_to_opt p x = SOME (p x) handle Overflow => NONE
    in
      local
	fun chars_to_int cs : Int32.int =
	  let fun chars_to_posint cs = Int32.fromLarge(Word32.toLargeInt(chars_to_w cs))
	  in case cs
	       of #"~" :: cs =>
		 (let (* be careful here! ~Int32.minInt is not representable *)
		    val w = chars_to_w cs
		  in if w = 0w2147483648 (* ~Int32.minInt *) then Option.valOf Int32.minInt
		     else ~ (chars_to_posint cs)
		  end handle _ => impossible "chars_to_int")
		| _ => chars_to_posint cs
	  end
      in
	val asInteger = exception_to_opt (chars_to_int o explode)
      end
      val asWord = exception_to_opt (asWord0 o explode)
      val chars_to_w_in_base = fn base => fn chars =>
	chars_to_w_in_base base chars
	handle Overflow => impossible "chars_to_w_in_base"

      fun asReal text =  (* the old code dealt incorrectly with 2147483647.0 *)
	case Real.fromString text
	  of SOME _ => SOME text  (* we test here if the text represents
				   * a real that is out of range. *)
	   | NONE => NONE

    end (*local*)

    fun initArg sourceReader = LEX_ARGUMENT{sourceReader=sourceReader,
					    stringChars=nil,
					    commentDepth=0}

    fun clearString arg = initArg(sourceReaderOf arg)

    fun newComment arg = LEX_ARGUMENT{sourceReader=sourceReaderOf arg,
				      stringChars=nil, commentDepth=1}

    fun addChars text (LEX_ARGUMENT{sourceReader, stringChars, ...}) =
      LEX_ARGUMENT{sourceReader=sourceReader,
		   stringChars=text :: stringChars, commentDepth=0}

    fun addControlChar text arg =
      addChars (str(chr(ord(String.sub(text,2)) - ord #"@"))) arg

    fun asDigit text = (ord(String.sub(text,0)) - ord #"0")
      handle _ => impossible "asDigit"

    local
      fun add_numbered_char (pos, text) arg limit n =
	if n > limit then
	  raise LexBasics.LEXICAL_ERROR
	    (pos, "ASCII escape " ^ text ^ " must be <= " ^ Int.toString limit)
	else
	  addChars (str(chr n)) arg
    in
      fun addAsciiChar (pos, text) arg =
	add_numbered_char (pos, text) arg 255
	(case explode text 
	   of [#"\\", c1, c2, c3] => (Word32.toInt(chars_to_w_in_base 0w10 [c1, c2, c3])
				      handle _ => impossible "addAsciiChar.Overflow")
	    | _ => impossible "addAsciiChar")

      (* Currently, `addUnicodeChar' will only allow `unicode' chars in
       * the range [0; 255].  20/06/1997 18:53. tho.*)

      fun addUnicodeChar (pos, text) arg =
	add_numbered_char (pos, text) arg 255
	(case explode text 
	   of [#"\\", #"u", c1, c2, c3, c4] =>
	     (Word32.toInt(chars_to_w_in_base 0w16 [c1, c2, c3, c4])
	      handle _ => impossible "addUnicodeChar.Overflow")
	    | _ => impossible "addUnicodeChar")

    end (*local*)

    fun asString(LEX_ARGUMENT{stringChars, ...}) = concat(rev stringChars)

   (* Keyword detection (better done here than by the lexer). *)

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
