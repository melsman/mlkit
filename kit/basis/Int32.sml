
structure Int32 : INTEGER = 
  struct (*Depends on StringCvt and Char*)

    (* Primitives *)
    fun quot(x:int32,y:int32) : int32 = if y = 0 then raise Div
					else prim ("__quot_int32", (x,y)) 
    fun rem(x:int32,y:int32) : int32 = if y = 0 then raise Div
				       else prim ("__rem_int32", (x,y))

    fun not true = false
      | not false = true

    (* Body *)
    fun toLarge (x: int32) : int32 = x
    fun fromLarge (x: int32) : int32 = x
    fun toInt (x: int32) : int = prim("__int32_to_int", x)
    fun fromInt (x: int) : int32 = prim("__int_to_int32", x)

    val precision = SOME 32

    val maxInt = SOME (2147483647 : int32)
    val minInt = SOME (~2147483648 : int32)

    val ~ : int32 -> int32 = ~  
    val op * : (int32 * int32) -> int32 = op * 
    val op div : (int32 * int32) -> int32 = op div 
    val op mod : (int32 * int32) -> int32 = op mod 
    val op + : (int32 * int32) -> int32 = op +
    val op - : (int32 * int32) -> int32 = op -
    fun compare (x, y: int32) = if x<y then LESS else if x>y then GREATER else EQUAL
    val abs     : int32 -> int32 = abs
    fun min (x, y) = if x < y then x else y : int32
    fun max (x, y) = if x < y then y else x : int32
    fun sign (i: int32) : int = if i > 0 then 1 else if i < 0 then ~1 else 0
    fun sameSign (i: int32, j) = sign i = sign j

    local
      open StringCvt
      fun ord32 (c:char) : int32 = fromInt(Char.ord c)
      fun chr32 (i:int32) : char = Char.chr(toInt i)

      (* Below, 48 = Char.ord #"0" and 55 = Char.ord #"A" - 10. *)
      fun hexval c = if #"0" <= c andalso c <= #"9" then ord32 c - 48
		     else (ord32 c - 55) mod 32
      fun prhex i = if i < 10 then chr32(i + 48) else chr32(i + 55)
      fun skipWSget getc source = getc (dropl Char.isSpace getc source)
	
      fun conv radix (i:int32) =
	if SOME i = minInt then          (* Be careful not to Overflow *)
	  (case radix
	     of 2 => "~10000000000000000000000000000000"
	      | 8 => "~20000000000"
	      | 10 => "~2147483648"
	      | 16 => "~80000000"
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
	    val (isDigit, factor:int32) =
	      case radix
		of BIN => (fn c => (#"0" <= c andalso c <= #"1"),  2)
		 | OCT => (fn c => (#"0" <= c andalso c <= #"7"),  8)
		 | DEC => (Char.isDigit,                          10)
		 | HEX => (Char.isHexDigit,                       16)
(*
	    fun dig1 sgn NONE = NONE
	      | dig1 sgn (SOME (c, rest)) =
	      let fun digr (res:int32) src =
		case getc src
		  of NONE => SOME (sgn * res, src)
		   | SOME (c, rest) => if isDigit c then digr (factor * res + hexval c) rest
				       else SOME (sgn * res, src)
	      in if isDigit c then digr (hexval c) rest else NONE 
	      end
*)
	    fun dig1 sgn NONE = NONE
	      | dig1 sgn (SOME (c, rest)) =
	      let fun digr (res:int32) next_val src =
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
	    fun hexopt (sgn:int32) NONE = NONE
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
      fun toString (i: int32): string = fmt DEC i
	
      fun fromString s = scanString (scan DEC) s
    end (*local*)

    val op >    : int32 * int32 -> bool = op >
    val op >=   : int32 * int32 -> bool = op >=
    val op <    : int32 * int32 -> bool = op <
    val op <=   : int32 * int32 -> bool = op <=

    type int = int32      

  end (*structure Int32*)

structure LargeInt : INTEGER = Int32
