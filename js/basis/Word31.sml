(* Requires Word32 to be defined *)

(** Operations on unsigned 31 bit word values. *)
structure Word31 : WORD =
  struct
    val wordSize = 31

    fun cast_wi (a: word) : int = prim("id", a)
    fun cast_iw (a: int) : word = prim("id", a)

    fun toInt (w : word31) : int = Word.toInt(prim("__word31_to_word", w))
    fun toIntX (w : word31) : int = prim("__word31_to_int_X_JS", w)
    fun fromInt (i : int) : word31 = prim("__int_to_word31_JS", i)

    fun toLargeWord (w : word31) : word32 = prim("__word31_to_word32", w)
    val toLarge = toLargeWord
    fun toLargeWordX (w : word31) : word32 = prim("__word31_to_word32_X", w)
    val toLargeX = toLargeWordX
    fun fromLargeWord (w : word32) : word31 = prim("__word32_to_word31", w)
    val fromLarge = fromLargeWord

    fun toLargeInt (w : word31) : intinf = 
	IntInfRep.fromWord31 w
    fun toLargeIntX (w : word31) : intinf = 
	IntInfRep.fromWord31X w
    fun fromLargeInt (i : intinf) : word31 = 
	IntInfRep.toWord31 i

    fun orb (x : word31, y : word31) : word31 = prim("__orb_word31", (x, y))
    fun andb (x : word31, y : word31) : word31 = prim("__andb_word31", (x, y))
    fun xorb (x : word31, y : word31) : word31 = prim("__xorb_word31", (x, y))
    fun notb (x : word31) : word31 = prim("__xorb_word31",  (x, fromInt ~1))

    local
      fun lshift_ (w : word31, k : word) : word31 = 
	prim("__shift_left_word31", (w,k))
      fun rshiftsig_ (w : word31, k : word) : word31 = 
	prim("__shift_right_signed_word31", (w,k))
      fun rshiftuns_ (w : word31, k : word) : word31 = 
	prim("__shift_right_unsigned_word31", (w,k))
    in
      fun << (w, k) = 
	if k >= cast_iw wordSize then 0w0
	else lshift_(w, k);

      fun >> (w, k) = 
	if k >= cast_iw wordSize then 0w0
	else rshiftuns_ (w, k)

      fun ~>> (w, k) = 
	if k >= cast_iw wordSize then 
	  if toIntX w >= 0 then 0w0 (* msbit = 0 *)
	  else fromInt ~1  (* msbit = 1 *)
	else rshiftsig_(w, k)
    end

    val op + = fn (w1:word31,w2) => w1 + w2
    val op - = fn (w1:word31,w2) => w1 - w2
    val op * = fn (w1:word31,w2) => w1 * w2
    val op div = fn (w1:word31,w2) => w1 div w2
    val op mod = fn (w1:word31,w2) => w1 mod w2

    val ~ = fn w => fromInt(~(toInt w)) 

    local 
      open StringCvt
      fun skipWSget getc source = getc (dropl Char.isSpace getc source)
      fun ord31 c = fromInt(Char.ord c)
      fun chr31 w = Char.chr (toInt w)
      (* Below, 48 = Char.ord #"0" and 55 = Char.ord #"A" - 10. *)
      fun hexval c = 
	if #"0" <= c andalso c <= #"9" then ord31 c - 0w48
	else (ord31 c - 0w55) mod 0w32;

      fun prhex i = if toInt i < 10 then chr31(i + 0w48)
		    else chr31(i + 0w55);

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
	      val (isDigit, factor : word31) = 
		  case radix of
		      BIN => (fn c => (#"0" <= c andalso c <= #"1"),  0w2)
		    | OCT => (fn c => (#"0" <= c andalso c <= #"7"),  0w8)
		    | DEC => (Char.isDigit,                           0w10)
		    | HEX => (Char.isHexDigit,                        0w16)
	      fun dig1 NONE              = NONE
		| dig1 (SOME (c1, src1)) = 
		  let fun digr res src = 
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

      fun fmt BIN = conv 0w2
	| fmt OCT = conv 0w8
	| fmt DEC = conv 0w10
	| fmt HEX = conv 0w16

      fun toString w   = conv 0w16 w
      fun fromString s = scanString (scan HEX) s
    end (* local for string functions *)

    fun min(w1 : word31, w2) = if w1 > w2 then w2 else w1;
    fun max(w1 : word31, w2) = if w1 > w2 then w1 else w2;
    fun compare (x, y: word31) = if x<y then LESS else if x>y then GREATER else EQUAL;

    val op > = fn (w1:word31,w2) => w1 > w2
    val op >= = fn (w1:word31,w2) => w1 >= w2
    val op < = fn (w1:word31,w2) => w1 < w2
    val op <= = fn (w1:word31,w2) => w1 <= w2

    type word = word31

  end
