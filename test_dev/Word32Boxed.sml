(* Boxed version of Word32 used when garbage collection is enabled. *)
structure Word32Boxed =  (* constrained to signature WORD in file WORD.sml well not yet 2001-02-17, Niels *) 
  struct
    type word = word_boxed (* boxed_word is a built in type *)

    val wordSize = 32

    fun toInt (w : word) : int = prim("toIntw32boxed__", "toIntw32boxed__", w)
    fun toIntX (w : word) : int = prim("toIntw32boxed__", "toIntw32boxed__", w)
    fun fromInt (i : int) : word = prim("fromIntw32boxed__", "fromIntw32boxed__", i)

    val a = fromInt 42
    val b = toInt a
    val _ = print (Int.toString b)
(*
    fun toLargeInt (w : word) : int = prim("fromInti32boxed", "id", #1 w)
    fun toLargeIntX (w : word) : int = prim("id", "id", #1 w)
    fun fromLargeInt (i : int) : word = prim("id", "id", i)

    fun toLargeWord (w : word) : word = w
    fun toLargeWordX (w : word) : word = w
    fun fromLargeWord (w : word) : word = w
*)

(*    fun orb (x : word, y : word) : word = prim("or_boxed__", "or_boxed__", (x, y))
    fun andb (x : word, y : word) : word = prim("and__", "and__", (x, y))
    fun xorb (x : word, y : word) : word = prim("xor__", "xor__", (x, y))
    fun notb (x : word) : word = prim("xor__", "xor__",  (x, fromInt ~1))*)
(*
    local
      fun lshift_ (w : word, k : word) : word = prim("shift_left__", "shift_left__", (w,k))
      fun rshiftsig_ (w : word, k : word) : word = prim("shift_right_signed__", "shift_right_signed__", (w,k))
      fun rshiftuns_ (w : word, k : word) : word = prim("shift_right_unsigned__", "shift_right_unsigned__", (w,k))
    in
      fun << (w, k) = 
	if toInt k >= wordSize orelse toInt k < 0 then fromInt 0
	else lshift_(w, k);

      fun >> (w, k) = 
	if toInt k >= wordSize orelse toInt k < 0 then fromInt 0
	else rshiftuns_ (w, k)

      fun ~>> (w, k) = 
	if toInt k >= wordSize orelse toInt k < 0 then 
	  if toInt w >= 0 then fromInt 0 (* msbit = 0 *)
	  else fromInt ~1  (* msbit = 1 *)
	else rshiftsig_(w, k)

    end

    val op + = fn (w1:word,w2) => w1 + w2
    val op - = fn (w1:word,w2) => w1 - w2
    val op * = fn (w1:word,w2) => w1 * w2
    val op div = fn (w1:word,w2) => w1 div w2
    val op mod = fn (w1:word,w2) => w1 mod w2

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
		      BIN => (fn c => (#"0" <= c andalso c <= #"1"),  2)
		    | OCT => (fn c => (#"0" <= c andalso c <= #"7"),  8)
		    | DEC => (Char.isDigit,                          10)
		    | HEX => (Char.isHexDigit,                       16)
	      fun dig1 NONE              = NONE
		| dig1 (SOME (c1, src1)) = 
		  let fun digr res src = 
		          case getc src of
			      NONE           => SOME (res, src)
			    | SOME (c, rest) => 
				  if isDigit c then 
				      digr (fromInt factor * res + hexval c) 
				      rest
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

    fun min(w1 : word, w2) = if w1 > w2 then w2 else w1;
    fun max(w1 : word, w2) = if w1 > w2 then w1 else w2;
    fun compare (x, y: word) = if x<y then LESS else if x>y then GREATER else EQUAL;

    val op > = fn (w1:word,w2) => w1 > w2
    val op >= = fn (w1:word,w2) => w1 >= w2
    val op < = fn (w1:word,w2) => w1 < w2
    val op <= = fn (w1:word,w2) => w1 <= w2
*)
  end