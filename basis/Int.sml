
structure Int : INTEGER =
  struct (*Depends on StringCvt and Char*)

    (* Primitives *)
    fun quot (x:int,y:int) : int = if y = 0 then raise Div
				   else prim ("__quot_int", (x, y))
    fun rem (x:int,y:int) : int = if y = 0 then raise Div
				  else prim ("__rem_int", (x, y))

    fun not true = false
      | not false = true

    (* Body *)
    type int = int
    fun toLarge (x: int) : intinf = IntInfRep.fromInt x
    fun fromLarge (x: intinf) : int = IntInfRep.toInt x
    fun toInt (x: int) : int = x
    fun fromInt (x: int) : int = x

    val precision = SOME Initial.precisionInt0

    val maxInt = SOME Initial.maxInt0
    val minInt = SOME Initial.minInt0

    val ~ : int -> int = ~
    val op * : (int * int) -> int = op *
    val op div : (int * int) -> int = op div
    val op mod : (int * int) -> int = op mod
    val op + : (int * int) -> int = op +
    val op - : (int * int) -> int = op -
    fun compare (x, y: int) = if x<y then LESS else if x>y then GREATER else EQUAL
    val abs     : int -> int = abs
    fun min (x, y) = if x < y then x else y : int
    fun max (x, y) = if x < y then y else x : int
    fun sign i = if i > 0 then 1 else if i < 0 then ~1 else 0
    fun sameSign (i, j) = sign i = sign j

    local
      open StringCvt
      (* Below, 48 = Char.ord #"0" and 55 = Char.ord #"A" - 10. *)
      fun decval c = Char.ord c - 48
      fun hexval c = if #"0" <= c andalso c <= #"9" then Char.ord c - 48
		     else (Char.ord c - 55) mod 32
      fun prhex i = if i < 10 then Char.chr(i + 48) else Char.chr(i + 55)
      fun skipWSget getc source = getc (dropl Char.isSpace getc source)
      fun conv rad radix i =
	if SOME i = minInt then          (* Be careful not to Overflow *)
	  let fun tag s1 s2 = if precision = SOME 63 then s1 else s2
	  in case rad
	       of BIN => tag "~100000000000000000000000000000000000000000000000000000000000000" "~1000000000000000000000000000000000000000000000000000000000000000"
		| OCT => tag "~400000000000000000000" "~1000000000000000000000"
		| DEC => tag "~4611686018427387904" "~9223372036854775808"
		| HEX => tag "~4000000000000000" "~8000000000000000"
	  end
	else
	  let fun h 0 res = res
		| h n res = h (n div radix) (prhex (n mod radix) :: res)
	      fun tostr n = h (n div radix) [prhex (n mod radix)]
	  in implode (if i < 0 then #"~" :: tostr (~i) else tostr i)
	  end
    in
      fun scan radix getc source =
	let val (isDigit, factor) =
	      case radix
		of BIN => (fn c => (#"0" <= c andalso c <= #"1"),  2)
		 | OCT => (fn c => (#"0" <= c andalso c <= #"7"),  8)
		 | DEC => (Char.isDigit,                          10)
		 | HEX => (Char.isHexDigit,                       16)
	    fun dig1 sgn NONE = NONE
	      | dig1 sgn (SOME (c, rest)) =
	      let fun digr (res:int) next_val src =
		case getc src
		  of NONE => SOME (res, src)
		   | SOME (c, rest) => if isDigit c then digr (next_val(factor, res, hexval c)) next_val rest
				       else SOME (res, src)
		  val next_val =
		    if sgn = 1 then fn (factor, res, hv) => factor * res + hv
		    else fn (factor, res, hv) => factor * res - hv
	      in if isDigit c then digr (sgn * hexval c) next_val rest else NONE
	      end
	    fun getdigs sgn after0 inp =
	      case dig1 sgn inp
		of NONE => SOME(0, after0)
		 | res  => res
	    fun hexopt sgn NONE = NONE
	      | hexopt sgn (SOME(#"0", after0)) =
	      if not(radix = HEX) then getdigs sgn after0 (getc after0)
	      else (case getc after0
		      of NONE => SOME(0, after0)
		       | SOME(#"x", rest) => getdigs sgn after0 (getc rest)
		       | SOME(#"X", rest) => getdigs sgn after0 (getc rest)
		       | inp => getdigs sgn after0 inp)
	      | hexopt sgn inp = dig1 sgn inp
	    fun sign NONE = NONE
	      | sign (SOME (#"~", rest)) = hexopt ~1 (getc rest)
	      | sign (SOME (#"-", rest)) = hexopt ~1 (getc rest)
	      | sign (SOME (#"+", rest)) = hexopt  1 (getc rest)
	      | sign inp = hexopt  1 inp
	in sign (skipWSget getc source)
	end

      fun fmt BIN = conv BIN 2
	| fmt OCT = conv OCT 8
	| fmt DEC = conv DEC 10
	| fmt HEX = conv HEX 16

      (* It should hold that: toString = fmt DEC = conv DEC 10 *)
      fun toString (i: int): string = fmt DEC i

      fun fromString s = scanString (scan DEC) s
    end (*local*)

    val op >    : int * int -> bool = op >
    val op >=   : int * int -> bool = op >=
    val op <    : int * int -> bool = op <
    val op <=   : int * int -> bool = op <=

  end (*structure Int*)

structure Position = Int
