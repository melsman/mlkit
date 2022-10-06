
structure Int31 : INTEGER =
  struct (*Depends on StringCvt and Char*)

    (* Primitives *)
    fun quot (x:int31,y:int31) : int31 = if y = 0 then raise Div
					 else prim ("__quot_int31", (x, y))
    fun rem (x:int31,y:int31) : int31 = if y = 0 then raise Div
				        else prim ("__rem_int31", (x,y))

    fun not true = false
      | not false = true

    (* Body *)
    fun toLarge (x: int31) : intinf = IntInfRep.fromInt31 x
    fun fromLarge (x: intinf) : int31 = IntInfRep.toInt31 x
    fun toInt (x: int31) : int = prim("__int31_to_int", x)
    fun fromInt (x: int) : int31 = prim("__int_to_int31", x)

    val precision = SOME 31

    val maxInt : int31 option = SOME 1073741823
    val minInt : int31 option = SOME ~1073741824

    val ~ : int31 -> int31 = ~
    val op * : (int31 * int31) -> int31 = op *
    val op div : (int31 * int31) -> int31 = op div
    val op mod : (int31 * int31) -> int31 = op mod
    val op + : (int31 * int31) -> int31 = op +
    val op - : (int31 * int31) -> int31 = op -
    fun compare (x, y: int31) = if x<y then LESS else if x>y then GREATER else EQUAL
    val abs     : int31 -> int31 = abs
    fun min (x, y) = if x < y then x else y : int31
    fun max (x, y) = if x < y then y else x : int31
    fun sign (i: int31) = if i > 0 then 1 else if i < 0 then ~1 else 0
    fun sameSign (i: int31, j) = sign i = sign j

    local
      open StringCvt
      fun ord31 (c : char) : int31 = fromInt (Char.ord c)
      fun chr31 (i : int31) : char = Char.chr (toInt i)
      (* Below, 48 = Char.ord #"0" and 55 = Char.ord #"A" - 10. *)
      fun decval c = ord31 c - 48
      fun hexval c = if #"0" <= c andalso c <= #"9" then ord31 c - 48
		     else (ord31 c - 55) mod 32
      fun prhex i = if i < 10 then chr31(i + 48) else chr31(i + 55)
      fun skipWSget getc source = getc (dropl Char.isSpace getc source)

      fun conv rad radix (i:int31) =
	if SOME i = minInt then          (* Be careful not to Overflow *)
	  (case rad
	     of BIN => "~1000000000000000000000000000000"
	      | OCT => "~10000000000"
	      | DEC => "~1073741824"
	      | HEX => "~40000000")
	else
	  let fun h 0 res = res
		| h n res = h (n div radix) (prhex (n mod radix) :: res)
	      fun tostr n = h (n div radix) [prhex (n mod radix)]
	  in implode (if i < 0 then #"~" :: tostr (~i) else tostr i)
	  end
    in
      fun scan radix getc source =
	let open StringCvt
	    val (isDigit, factor) =
	      case radix
		of BIN => (fn c => (#"0" <= c andalso c <= #"1"),  2)
		 | OCT => (fn c => (#"0" <= c andalso c <= #"7"),  8)
		 | DEC => (Char.isDigit,                          10)
		 | HEX => (Char.isHexDigit,                       16)
	    fun dig1 sgn NONE = NONE
	      | dig1 sgn (SOME (c, rest)) =
	      let fun digr (res:int31) next_val src =
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
      fun toString (i: int31): string = fmt DEC i

      fun fromString s = scanString (scan DEC) s
    end (*local*)

    val op >    : int31 * int31 -> bool = op >
    val op >=   : int31 * int31 -> bool = op >=
    val op <    : int31 * int31 -> bool = op <
    val op <=   : int31 * int31 -> bool = op <=

    type int = int31
  end (*structure Int*)
