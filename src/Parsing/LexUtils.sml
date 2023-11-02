(* General lexing utilities. *)

functor LexUtils(Token: Topdec_TOKENS): LEX_UTILS =
  struct

    open LexBasics Token
    fun impossible s = Crash.impossible ("LexUtils." ^ s)
    fun noSome NONE s = impossible s
      | noSome (SOME x) s = x

    datatype LexArgument = LEX_ARGUMENT of {sourceReader: SourceReader,
					    stringChars: string list ref,
					    commentDepth: int ref,
					    parStack: int ref list ref
					   }

    fun sourceReaderOf (LEX_ARGUMENT{sourceReader, ...}) = sourceReader

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
      fun ordw c = IntInf.fromInt(ord c)

      fun chars_to_w (#"0" :: #"x" :: chars) = chars_to_w_in_base 16 chars
	| chars_to_w chars = chars_to_w_in_base 10 chars
      and chars_to_w_in_base base chars = chars_to_w_in_base0 base 0 chars
      and chars_to_w_in_base0 base n [] = n
	| chars_to_w_in_base0 base n (char :: chars) =
	    (case char_to_w_opt base char of
	       SOME i =>
		 let val new = n * base + i
		 in chars_to_w_in_base0 base new chars
		 end
	     | NONE => n)
      and char_to_w_opt base char =
	let val i = if Char.isUpper char then ordw char - ordw #"A" + 10
		    else if Char.isLower char then ordw char - ordw #"a" + 10
			 else if Char.isDigit char then ordw char - ordw #"0"
			      else base (*hack*)
	in
	  if i<base then SOME i else NONE
	end handle _ => NONE

      fun asWord0 (#"0" :: #"w" :: #"x" :: chars) = chars_to_w_in_base 16 chars
	| asWord0 (#"0" :: #"w" :: chars) = chars_to_w_in_base 10 chars
	| asWord0 _ = impossible "asWord0"

      fun exception_to_opt p x = SOME (p x) handle Overflow => NONE
    in
      local
	  fun ordi c = IntInf.fromInt(ord c)

	  fun chars_to_i (#"0" :: #"x" :: chars) = chars_to_i_in_base (IntInf.fromInt 16) chars
	    | chars_to_i chars = chars_to_i_in_base (IntInf.fromInt 10) chars
	  and chars_to_i_in_base base chars = chars_to_i_in_base0 base (IntInf.fromInt 0) chars
	  and chars_to_i_in_base0 base n [] = n
	    | chars_to_i_in_base0 base n (char :: chars) =
	      (case char_to_i_opt char of
		   SOME i =>
		       let val new = IntInf.+(IntInf.*(n, base), i)
		       in chars_to_i_in_base0 base new chars
		       end
		 | NONE => n)
	  and char_to_i_opt char =
	      let val i = if Char.isUpper char then SOME(IntInf.+(IntInf.-(ordi char, ordi #"A"), IntInf.fromInt 10))
			  else if Char.isLower char then SOME(IntInf.+(IntInf.-(ordi char, ordi #"a"), IntInf.fromInt 10))
			    else if Char.isDigit char then SOME(IntInf.-(ordi char, ordi #"0"))
				 else NONE
	      in i
	      end handle _ => NONE

	fun chars_to_int cs : IntInf.int =
	    case cs of
		#"~" :: cs => IntInf.~ (chars_to_i cs)
	      | _ => chars_to_i cs
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
					    stringChars=ref nil,
					    commentDepth=ref 0,
					    parStack=ref nil}

    fun clearString (LEX_ARGUMENT{stringChars,...}) = stringChars := nil

    fun newComment (LEX_ARGUMENT{commentDepth,...}) = commentDepth := 1

    fun addChars text (LEX_ARGUMENT{stringChars,...}) =
      stringChars := text :: (!stringChars)

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
	   of [#"\\", c1, c2, c3] => (IntInf.toInt(chars_to_w_in_base 10 [c1, c2, c3])
				      handle _ => impossible "addAsciiChar.Overflow")
	    | _ => impossible "addAsciiChar")

      (* Currently, `addUnicodeChar' will only allow `unicode' chars in
       * the range [0; 255].  20/06/1997 18:53. tho.*)

      fun addUnicodeChar (pos, text) arg =
	add_numbered_char (pos, text) arg 255
	(case explode text
	   of [#"\\", #"u", c1, c2, c3, c4] =>
	     (IntInf.toInt(chars_to_w_in_base 16 [c1, c2, c3, c4])
	      handle _ => impossible "addUnicodeChar.Overflow")
	    | _ => impossible "addUnicodeChar")

    end (*local*)

    fun asString (LEX_ARGUMENT{stringChars, ...}) = concat(rev (!stringChars))

    val is_reml =
        let val reml = ref false
        in Flags.add_bool_entry
               {long="reml", short=NONE, item=reml,
	        menu=["General Control", "ReML"], neg=false,
	        desc="ReML is Standard ML with support for programming with \n\
                     \explicit regions, explicit effects, and effect \n\
                     \constraints. With ReML, atomic effects also include \n\
                     \mutation effects. Whereas ReML include parallel \n\
                     \thread support, ReML does not support integration \n\
                     \with reference-tracing garbage collection."}
        end

   (* Keyword detection (better done here than by the lexer). *)

    fun identifier (text, p1, p2) =
      let
	fun keyword tok = tok(p1, p2)
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
	   | "*"	 => keyword STAR    (* Not actually reserved, but ... *)

	   | _		 => if is_reml() then
                              if text = "`" then keyword BACKQUOTE
                              else if text = "##" then keyword HASHHASH
                              else ID(text, p1, p2)
                            else ID(text, p1, p2)
      end

    fun incComment (LEX_ARGUMENT{commentDepth,...}) =
      commentDepth := !commentDepth + 1

    fun decComment (LEX_ARGUMENT{commentDepth,...}) =
      (commentDepth := !commentDepth - 1;
       !commentDepth)

    (* support for quotations (frags) *)
    fun parStackTop (LEX_ARGUMENT {parStack,...}) =
      case !parStack
	of (r::_) => r
	 | _ => raise Fail "LexUtils.parStackTop"
    fun parStackPush (r : int ref) (LEX_ARGUMENT {parStack,...}) : unit =
      parStack := r :: !parStack
    fun parStackPop (LEX_ARGUMENT {parStack,...}) : unit =
      case !parStack
	of (r::t) => parStack := t
	 | _ => raise Fail "LexUtils.parStackPop"
    fun parStackIsEmpty (LEX_ARGUMENT {parStack,...}) : bool =
      case !parStack
	of nil => true
	 | _ => false

  end
