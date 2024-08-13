structure Word64 : WORD =
  struct
    val wordSize = 64

    val wordSize_w : word = 0w64

    fun cast_iw (a: int) : word = prim("id", a)

    fun toInt (w : word64) : int = prim("__word64_to_int", w)
    fun toIntX (w : word64) : int = prim("__word64_to_int_X", w)
    fun fromInt (x: int) : word64 = prim("__int_to_int64", x)

    fun toLargeWord (w : word64) : word64 = w
    val toLarge = toLargeWord
    fun toLargeWordX (w : word64) : word64 = w
    val toLargeX = toLargeWordX
    fun fromLargeWord (w : word64) : word64 = w
    val fromLarge = fromLargeWord

    fun toLargeInt (w : word64) : intinf =
	IntInfRep.fromWord64 w

    fun toLargeIntX (w : word64) : intinf =
	IntInfRep.fromWord64X w

    fun fromLargeInt (i : intinf) : word64 =
	IntInfRep.toWord64 i

    fun orb (x : word64, y : word64) : word64 = prim("__orb_word64", (x, y))
    fun andb (x : word64, y : word64) : word64 = prim("__andb_word64", (x, y))
    fun xorb (x : word64, y : word64) : word64 = prim("__xorb_word64", (x, y))
    fun notb (x : word64) : word64 = prim("__xorb_word64", (x, fromInt ~1))    (* MEMO *)

    local
      fun lshift_ (w : word64, k : word) : word64 =
	  prim("__shift_left_word64", (w,k))
      fun rshiftsig_ (w : word64, k : word) : word64 =
	  prim("__shift_right_signed_word64", (w,k))
      fun rshiftuns_ (w : word64, k : word) : word64 =
	  prim("__shift_right_unsigned_word64", (w,k))
(*
      fun toInt64X (w : word64) : int64 =
	  prim("__word64_to_int64_X", w)
*)
    in
      fun << (w, k)  = if k >= wordSize_w then 0w0
		       else lshift_(w, k)

      fun >> (w, k) = if k >= wordSize_w then 0w0
		      else rshiftuns_(w, k)

      fun ~>> (w, k) =
        if k >= wordSize_w then
            rshiftsig_(w, wordSize_w-0w1)
(*
	  if toInt64X w >= 0 then 0w0    (* msbit = 0 *)
	  else fromInt ~1                (* msbit = 1 *)
*)
	else rshiftsig_(w, k)
    end

    val op + = fn (w1:word64,w2) => w1 + w2
    val op - = fn (w1:word64,w2) => w1 - w2
    val op * = fn (w1:word64,w2) => w1 * w2
    val op div = fn (w1:word64,w2) => w1 div w2
    val op mod = fn (w1:word64,w2) => w1 mod w2

    val ~ = fn w => notb w + 0w1

    local
      open StringCvt
      fun skipWSget getc source = getc (dropl Char.isSpace getc source)

      (* Below, 48 = Char.ord #"0" and 55 = Char.ord #"A" - 10. *)
      fun decval c = fromInt (Char.ord c) - fromInt 48;
      fun hexval c =
	  if #"0" <= c andalso c <= #"9" then
	      fromInt (Char.ord c) - fromInt 48
	  else
	      (fromInt (Char.ord c) - fromInt 55) mod (fromInt 32);

      fun prhex i =
	  if toInt i < 10 then Char.chr(toInt (i + fromInt 48))
	  else Char.chr(toInt (i + fromInt 55));

      fun conv radix i =
	  let fun h n res =
		  if n = fromInt 0 then res
		  else h (n div radix) (prhex (n mod radix) :: res)
	      fun tostr n = h (n div radix) [prhex (n mod radix)]
	  in String.implode (tostr i) end

    in
      fun scan radix getc source =
	  let open StringCvt
	      val source = skipWS getc source
	      val (isDigit, factor) =
		  case radix of
		      BIN => (fn c => (#"0" <= c andalso c <= #"1"),  0w2)
		    | OCT => (fn c => (#"0" <= c andalso c <= #"7"),  0w8)
		    | DEC => (Char.isDigit,                          0w10)
		    | HEX => (Char.isHexDigit,                       0w16)
	      fun dig1 NONE              = NONE
		| dig1 (SOME (c1, src1)) =
		  let fun digr (res:word64) src =
		          case getc src of
			      NONE           => SOME (res, src)
			    | SOME (c, rest) =>
				if isDigit c then
				  let val res1 = factor * res
				    val res2 = res1 + hexval c
				  in if res1 < res orelse res2 < res1 then raise Overflow
				     else digr res2 rest
				  end
				else SOME (res, src)
		  in
		      if isDigit c1 then digr (hexval c1) src1
		      else NONE
		  end
	      fun getdigs after0 src =
		  case dig1 (getc src) of
		      NONE => SOME(fromInt 0, after0)
		    | res  => res
	      fun hexprefix after0 src =
		  if radix <> HEX then getdigs after0 src
		  else
		      case getc src of
			  SOME(#"x", rest) => getdigs after0 rest
			| SOME(#"X", rest) => getdigs after0 rest
			| SOME _           => getdigs after0 src
			| NONE => SOME(fromInt 0, after0)
	  in
	      case getc source of
		  SOME(#"0", after0) =>
		      (case getc after0 of
			   SOME(#"w", src2) => hexprefix after0 src2
			 | SOME _           => hexprefix after0 after0
			 | NONE             => SOME(fromInt 0, after0))
		| SOME _ => dig1 (getc source)
		| NONE   => NONE
	  end;

      fun fmt BIN = conv (fromInt  2)
	| fmt OCT = conv (fromInt  8)
	| fmt DEC = conv (fromInt 10)
	| fmt HEX = conv (fromInt 16)

      fun toString w   = conv (fromInt 16) w
      fun fromString s = scanString (scan HEX) s
    end (* local for string functions *)

    fun min (w1 : word64, w2) = if w1 > w2 then w2 else w1;
    fun max (w1 : word64, w2) = if w1 > w2 then w1 else w2;
    fun compare (x, y: word64) = if x<y then LESS else if x>y then GREATER else EQUAL;

    val op > = fn (w1:word64,w2) => w1 > w2
    val op >= = fn (w1:word64,w2) => w1 >= w2
    val op < = fn (w1:word64,w2) => w1 < w2
    val op <= = fn (w1:word64,w2) => w1 <= w2

    type word = word64
  end

(** SigDoc *)
structure LargeWord : WORD = Word64
