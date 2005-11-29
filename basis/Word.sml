
structure Word : WORD =
  struct
    type word = word

    val wordSize = Initial.precisionInt0

    fun toIntX (w : word) : int = prim("id", w)
    fun toInt (w : word) : int = 
      let val i = toIntX w
      in if i < 0 then raise Overflow
	 else i
      end

    fun fromInt (i : int) : word = prim("id", i)

    fun wordSize_w() = fromInt wordSize

    fun toLargeWord (w : word) : word32 = prim("__word_to_word32", w)

    fun toLargeInt (w : word) : int32 = prim("__word32_to_int32", toLargeWord w)

    fun toLargeIntX (w : word) : int32 = prim("__int_to_int32", toIntX w)

    fun fromLargeInt (i : int32) : word = prim("__int32_to_word", i)


    fun toLargeWordX (w : word) : word32 = prim("__word_to_word32_X", w)
    fun fromLargeWord (w : word32) : word = prim("__word32_to_word", w)

    fun orb (x : word, y : word) : word = prim("__orb_word", (x, y))
    fun andb (x : word, y : word) : word = prim("__andb_word", (x, y))
    fun xorb (x : word, y : word) : word = prim("__xorb_word", (x, y))
    fun notb (x : word) : word = xorb(x, fromInt ~1)

    local
      fun lshift_ (w : word, k : word) : word = prim("__shift_left_word", (w,k))
      fun rshiftsig_ (w : word, k : word) : word = 
	prim("__shift_right_signed_word", (w,k))
      fun rshiftuns_ (w : word, k : word) : word = 
	prim("__shift_right_unsigned_word", (w,k))
    in
      fun << (w, k) = if k >= wordSize_w() then 0w0
		      else lshift_(w, k)

      fun >> (w, k) = if k >= wordSize_w() then 0w0
		      else rshiftuns_ (w, k)

      fun ~>> (w, k) = 
	if k >= wordSize_w() then 
	  if toIntX w >= 0 then 0w0   (* msbit = 0 *)
	  else fromInt ~1             (* msbit = 1 *)
	else rshiftsig_(w, k)

    end

    val op + = fn (w1:word,w2) => w1 + w2
    val op - = fn (w1:word,w2) => w1 - w2
    val op * = fn (w1:word,w2) => w1 * w2
    val op div = fn (w1:word,w2) => w1 div w2
    val op mod = fn (w1:word,w2) => w1 mod w2

    val ~ = fn w => fromInt(~(toInt w)) 

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

  end

structure SysWord : WORD = Word
