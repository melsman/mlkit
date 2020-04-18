
structure Int64 : INTEGER =
  struct (*Depends on StringCvt and Char*)

    (* Primitives *)
    fun quot (x:int64,y:int64) : int64 = if y = 0 then raise Div
					 else prim ("__quot_int64", (x,y))
    fun rem (x:int64,y:int64) : int64 = if y = 0 then raise Div
				        else prim ("__rem_int64", (x,y))

    fun not true = false
      | not false = true

    (* Body *)
    fun toLarge (x: int64) : intinf = IntInfRep.fromInt64 x
    fun fromLarge (x: intinf) : int64 = IntInfRep.toInt64 x
    fun toInt (x: int64) : int = prim("__int64_to_int", x)
    fun fromInt (x: int) : int64 = prim("__int_to_int64", x)

    val precision = SOME 64

    val maxInt = SOME (7*7*73*127*337*92737*649657 : int64)                (*  9223372036854775807 = 2^63-1 = 7*7*73*127*337*92737*649657 *)
    val minInt = SOME (~(7*7*73*127*337*92737*649657) - 1 : int64)         (* ~9223372036854775808 = ~2^63 = ~(7*7*73*127*337*92737*649657) - 1 *)

    val ~ : int64 -> int64 = ~
    val op * : (int64 * int64) -> int64 = op *
    val op div : (int64 * int64) -> int64 = op div
    val op mod : (int64 * int64) -> int64 = op mod
    val op + : (int64 * int64) -> int64 = op +
    val op - : (int64 * int64) -> int64 = op -
    fun compare (x, y: int64) = if x<y then LESS else if x>y then GREATER else EQUAL
    val abs     : int64 -> int64 = abs
    fun min (x, y) = if x < y then x else y : int64
    fun max (x, y) = if x < y then y else x : int64
    fun sign (i: int64) : int = if i > 0 then 1 else if i < 0 then ~1 else 0
    fun sameSign (i: int64, j) = sign i = sign j

    local
      open StringCvt
      fun ord64 (c:char) : int64 = fromInt(Char.ord c)
      fun chr64 (i:int64) : char = Char.chr(toInt i)

      (* Below, 48 = Char.ord #"0" and 55 = Char.ord #"A" - 10. *)
      fun hexval c = if #"0" <= c andalso c <= #"9" then ord64 c - 48
		     else (ord64 c - 55) mod 64
      fun prhex i = if i < 10 then chr64(i + 48) else chr64(i + 55)
      fun skipWSget getc source = getc (dropl Char.isSpace getc source)

      fun conv radix (i:int64) =
	if SOME i = minInt then          (* Be careful not to Overflow *)
	  (case radix
	     of 2 => "~10000000000000000000000000000000" ^ "00000000" ^ "00000000" ^ "00000000" ^ "00000000"
	      | 8 => "~20000000000" ^ "00000000000"
	      | 10 => "~9223372036854775808"
	      | 16 => "~80000000" ^ "00000000"
	      | _ => raise Fail "conv")
	else
	  let fun h 0 res = res
		| h n res = h (n div radix) (prhex (n mod radix) :: res)
	      fun tostr n = h (n div radix) [prhex (n mod radix)]
	  in implode (if i < 0 then #"~" :: tostr (~i) else tostr i)
	  end
    in
      fun scan radix getc source =
	let open StringCvt
	    val (isDigit, factor:int64) =
	      case radix
		of BIN => (fn c => (#"0" <= c andalso c <= #"1"),  2)
		 | OCT => (fn c => (#"0" <= c andalso c <= #"7"),  8)
		 | DEC => (Char.isDigit,                          10)
		 | HEX => (Char.isHexDigit,                       16)
	    fun dig1 sgn NONE = NONE
	      | dig1 sgn (SOME (c, rest)) =
	      let fun digr (res:int64) next_val src =
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
	    fun hexopt (sgn:int64) NONE = NONE
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
      fun toString (i: int64): string = fmt DEC i

      fun fromString s = scanString (scan DEC) s
    end (*local*)

    val op >    : int64 * int64 -> bool = op >
    val op >=   : int64 * int64 -> bool = op >=
    val op <    : int64 * int64 -> bool = op <
    val op <=   : int64 * int64 -> bool = op <=

    type int = int64

  end (*structure Int64*)

(*structure FixedInt : INTEGER = Int64*)
