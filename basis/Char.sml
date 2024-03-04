
structure Char =
struct (* depends on StrBase *)

  (* Primitives *)
  fun sub_unsafe (s:string,i:int) : char = prim ("__bytetable_sub", (s,i))
  fun size (s:string): int = prim ("__bytetable_size", s)
  fun ord (c : char) : int = prim ("ord", c)

  fun unsafe_chr (i:int):char = prim ("id", i)

  fun chr (i:int) : char =
    if i>=0 andalso i<256 then prim ("id", i)
    else raise Chr

  (* Body *)

  type char = char   (* Invariant: for c: char it holds that 0 <= ord c <= maxOrd *)
   and string = string

  fun not true = false
    | not false = true

  val minChar = #"\000"
  val maxChar = #"\255"
  val maxOrd = 255

  fun succ c =
    let val i = ord c
    in if i < 255 then unsafe_chr(i + 1)
       else raise Chr
    end

  fun pred c =
    let val i = ord c
    in if i > 0 then unsafe_chr(i - 1)
       else raise Chr
    end

  fun contains (s:string) (c:char) : bool =
    let
      val sz = size s
      fun loop j = if j >= sz then false
		   else sub_unsafe(s,j) = c orelse loop (j+1)
    in loop 0
    end

  fun notContains s c = not(contains s c)

  fun isAscii c  = c <= #"\127"

  fun isUpper c  = #"A" <= c andalso c <= #"Z"
  fun isLower c  = #"a" <= c andalso c <= #"z"
  fun isDigit c  = #"0" <= c andalso c <= #"9"
  fun isAlpha c  = isUpper c orelse isLower c
  fun isAlphaNum c = isAlpha c orelse isDigit c
  fun isHexDigit c = isDigit c
	             orelse #"a" <= c andalso c <= #"f"
	             orelse #"A" <= c andalso c <= #"F"
  fun isGraph c  = #"!" <= c andalso c <= #"~"
  fun isPrint c  = isGraph c orelse c = #" "
  fun isPunct c  = isGraph c andalso not (isAlphaNum c)
  fun isCntrl c  = isAscii c andalso not (isPrint c)
  fun isSpace c  = #"\t" <= c andalso c <= #"\r"
                   orelse c = #" "

  fun toLower c = if isUpper c then unsafe_chr(ord c + 32)
		  else c
  fun toUpper c = if isLower c then unsafe_chr(ord c - 32)
		  else c

  fun toString c = StrBase.toMLescape c

  fun scan getc s =
    case getc s
	of NONE => NONE
	 | SOME(#"\\", rest) => (case StrBase.fromMLescape getc rest
				   of NONE => NONE
				    | SOME res => SOME res)
	 | SOME res => SOME res


  fun fromString s =
    let fun getc i = if i < size s then SOME (sub_unsafe(s, i), i+1) else NONE
    in case getc 0
	   of NONE => NONE
	    | SOME(#"\\", rest) => (case StrBase.fromMLescape getc rest
				      of NONE => NONE
				       | SOME(c, _) => SOME c)
	    | SOME(c, _ ) => SOME c
    end

  fun fromCString s =
    let fun getc i = if i < size s then SOME (sub_unsafe(s, i), i+1) else NONE
    in case getc 0
	   of NONE => NONE
	    | SOME(#"\\", rest) => (case StrBase.fromCescape getc rest
				      of NONE => NONE
				       | SOME(c, _) => SOME c)
	    | SOME(c, _ ) => SOME c
    end

  fun toCString c = StrBase.toCescape c

  fun compare (x, y: char) = if x<y then LESS else if x>y then GREATER else EQUAL

  val op <  = op <  : char * char -> bool
  val op <= = op <= : char * char -> bool
  val op >  = op >  : char * char -> bool
  val op >= = op >= : char * char -> bool
end (*structure Char*)
