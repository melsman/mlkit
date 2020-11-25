
structure Int63 : INTEGER =
  struct (*Depends on StringCvt and Char*)

    (* Primitives *)
    fun quot (x:int63,y:int63) : int63 = if y = 0 then raise Div
					 else prim ("__quot_int63", (x, y))
    fun rem (x:int63,y:int63) : int63 = if y = 0 then raise Div
				        else prim ("__rem_int63", (x,y))

    fun not true = false
      | not false = true

    (* Body *)
    fun toLarge (x: int63) : intinf = IntInfRep.fromInt63 x
    fun fromLarge (x: intinf) : int63 = IntInfRep.toInt63 x
    fun toInt (x: int63) : int = prim("__int63_to_int", x)
    fun fromInt (x: int) : int63 = prim("__int_to_int63", x)

    val precision = SOME 63

    val maxInt : int63 option = SOME Initial.maxInt63
    val minInt : int63 option = SOME Initial.minInt63

    val ~ : int63 -> int63 = ~
    val op * : (int63 * int63) -> int63 = op *
    val op div : (int63 * int63) -> int63 = op div
    val op mod : (int63 * int63) -> int63 = op mod
    val op + : (int63 * int63) -> int63 = op +
    val op - : (int63 * int63) -> int63 = op -
    fun compare (x, y: int63) = if x<y then LESS else if x>y then GREATER else EQUAL
    val abs     : int63 -> int63 = abs
    fun min (x, y) = if x < y then x else y : int63
    fun max (x, y) = if x < y then y else x : int63
    fun sign (i: int63) = if i > 0 then 1 else if i < 0 then ~1 else 0
    fun sameSign (i: int63, j) = sign i = sign j

    local
      open StringCvt
      fun ord63 (c : char) : int63 = fromInt (Char.ord c)
      fun chr63 (i : int63) : char = Char.chr (toInt i)
      (* Below, 48 = Char.ord #"0" and 55 = Char.ord #"A" - 10. *)
      fun decval c = ord63 c - 48
      fun hexval c = if #"0" <= c andalso c <= #"9" then ord63 c - 48
		     else (ord63 c - 55) mod 32
      fun prhex i = if i < 10 then chr63(i + 48) else chr63(i + 55)
      fun skipWSget getc source = getc (dropl Char.isSpace getc source)
      fun conv radix (i:int63) =
	  if SOME i = minInt then          (* Be careful not to Overflow *)
	    (case radix of
                 2 => "~100000000000000000000000000000000000000000000000000000000000000"
	       | 8 => "~400000000000000000000"
	       | 10 => "~4611686018427387904"
	       | 16 => "~4000000000000000"
               | _ => raise Fail "Int63.conv")
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
	      let fun digr (res:int63) next_val src =
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

      fun fmt BIN = conv 2
	| fmt OCT = conv 8
	| fmt DEC = conv 10
	| fmt HEX = conv 16

      (* It should hold that: toString = fmt DEC = conv 10 *)
      fun toString (i: int63): string = fmt DEC i

      fun fromString s = scanString (scan DEC) s
    end (*local*)

    val op >    : int63 * int63 -> bool = op >
    val op >=   : int63 * int63 -> bool = op >=
    val op <    : int63 * int63 -> bool = op <
    val op <=   : int63 * int63 -> bool = op <=

    type int = int63
  end (*structure Int*)
