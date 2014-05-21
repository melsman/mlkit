(* Requires Word, Word32 *)

(** Unsigned 8-bit word value operations. *)
structure Word8 : WORD =
  struct

    fun lshift_w (w: word,i: word): word = prim("__shift_left_word", (w,i))
    fun rshiftuns_w (w: word,i: word): word = prim("__shift_right_unsigned_word", (w,i))
    fun w_w8 (w : word) : word8 = prim("id", w)
    fun orb_w (x: word, y: word) : word = prim ("__orb_word", (x,y))
    fun xorb_w (x: word, y: word) : word = prim ("__xorb_word", (x,y))
    fun andb_w(x:word,y:word):word = prim ("__andb_word", (x,y))
    fun i_w (i : int) : word = prim("id", i)
    fun w_i (w : word) : int = prim("id", w)
    fun w8_w (w : word8) : word = prim("id", w)
    fun toInt (x : word8) : int = prim ("id", x)

    fun toLargeWord (w: word8) : word32 = prim ("__word_to_word32", w8_w w)
    val toLarge = toLargeWord

    fun toLargeInt (w: word8) : intinf = 
	IntInfRep.fromWord8 w
      
    fun norm (w: word) : word = andb_w (0w255, w)
    fun fromLargeWord (w: word32) : word8 = w_w8(norm(prim ("__word32_to_word", w)))
    val fromLarge = fromLargeWord
	
    (* The rest does not make use of prim *)

    val wordSize = 8

    fun fromInt (x : int) : word8 = w_w8(norm(i_w x))

    val ~ = fn w => fromInt(~(toInt w)) 
      
    (* Invariant for values w of type Word8.word: 0 <= toInt w < 256 *)

    fun andb (x: word8, y: word8) : word8 = 
      w_w8(andb_w(w8_w x, w8_w y))
      
    fun orb (x: word8, y: word8) : word8 = 
      w_w8(orb_w(w8_w x, w8_w y))

    fun xorb (x: word8, y: word8) : word8 = 
      w_w8(norm(xorb_w (w8_w x, w8_w y)))
	
    fun toIntX w = if w < 0w128 then toInt w   (* msbit = 0 *) 
		   else toInt w - 256          (* msbit = 1 *)
	  
    fun toLargeIntX (w : word8) : intinf = 
	IntInfRep.fromWord8X w

    fun fromLargeInt (i:intinf) : word8 = 
      w_w8 (norm (Word.fromLargeInt i))
	
    fun toLargeWordX (w: word8) = 
      if w < 0w128 then toLargeWord w               (* msbit = 0 *)
      else Word32.orb(toLargeWord w, 0wxFFFFFF00)   (* msbit = 1 *)
    val toLargeX = toLargeWordX

    fun notb x = xorb(x, 0wxFF) 
      
    fun << (w:word8, k:word) = 
      if k >= 0w8 then 0w0
      else w_w8(norm(lshift_w (w8_w w, k)))

    fun >> (w:word8, k:word) = 
      if k >= 0w8 then 0w0
      else w_w8(rshiftuns_w (w8_w w, k))    (* normalization not necessary *)
	  
    fun ~>> (w, k) = 
      if w < 0w128 then	(* msbit = 0: no sign to extend  *)
	if k >= 0w8 then 0w0
	else >> (w, k)
      else			(* msbit = 1: extend, then shift *)
	if k >= 0w8 then  0wxFF
	else w_w8(norm(rshiftuns_w ((orb_w (w8_w w, 0wxFF00)), k)))
	    
    local 
      open StringCvt
      fun skipWSget getc source = getc (skipWS getc source)
	
      (* Below, 48 = Char.ord #"0" and 55 = Char.ord #"A" - 10. *)
      fun decval c = Char.ord c - 48
      fun hexval c = Word.fromInt
	(if #"0" <= c andalso c <= #"9" then decval c
	 else (Char.ord c - 55) mod 32)
	   
      fun prhex i = 
	if i < 10 then Char.chr(i + 48) else Char.chr(i + 55);
	  
      fun conv radix w = 
	let 
	  fun h n res = 
	    if n = 0 then res
	    else h (n div radix) (prhex (n mod radix) :: res)
	  fun tostr n = h (n div radix) [prhex (n mod radix)]
	in String.implode (tostr (toInt w)) 
	end
      
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
	  fun return res src = 
	    if res <= 0w255 then SOME (w_w8 res, src) 
	    else raise Overflow
	  fun dig1 NONE             = NONE
	    | dig1 (SOME (c, rest)) = 
	    let 
	      fun digr res src = 
		case getc src of
		  NONE           => return res src
		| SOME (c, rest) => 
		    if isDigit c then
		      let val res1 = factor * res
			  val res2 = res1 + hexval c
		      in
			if res1 < res orelse res2 < res1 then raise Overflow
			else digr res2 rest
		      end
		    else 
		      return res src
	    in 
	      if isDigit c then digr (hexval c) rest else NONE 
	    end
	  fun getdigs after0 src = 
	    case dig1 (getc src) of
	      NONE => return 0w0 after0
	    | res  => res
	  fun hexprefix after0 src =
	    if radix <> HEX then getdigs after0 src
	    else
	      case getc src of
		SOME(#"x", rest) => getdigs after0 rest
	      | SOME(#"X", rest) => getdigs after0 rest
	      | SOME _           => getdigs after0 src
	      | NONE => return 0w0 after0
	in 
	  case getc source of
	    SOME(#"0", after0) => 
	      (case getc after0 of 
		 SOME(#"w", src2) => hexprefix after0 src2 
	       | SOME _           => hexprefix after0 after0 
	       | NONE             => return 0w0 after0)
	  | SOME _ => dig1 (getc source)
	  | NONE   => NONE 
	end;
	
      fun fmt BIN = conv  2
	| fmt OCT = conv  8
	| fmt DEC = conv 10
	| fmt HEX = conv 16
      fun toString w   = conv 16 w
      fun fromString s = scanString (scan HEX) s
    end (* local for string functions *)
  
    (* Redefining +, -, *, div, and mod is a horrible idea ... *)
    
    val op + = fn (w1:word8,w2) => w1 + w2
    val op - = fn (w1:word8,w2) => w1 - w2
    val op * = fn (w1:word8,w2) => w1 * w2
    val op div = fn (w1:word8,w2) => w1 div w2
    val op mod = fn (w1:word8,w2) => w1 mod w2

    fun min(w1 : word8, w2) = if w1 > w2 then w2 else w1
    fun max(w1 : word8, w2) = if w1 > w2 then w1 else w2
    fun compare (x, y: word8) = if x<y then LESS else if x>y then GREATER else EQUAL;
    val op > = fn (w1:word8,w2) => w1 > w2
    val op >= = fn (w1:word8,w2) => w1 >= w2
    val op < = fn (w1:word8,w2) => w1 < w2
    val op <= = fn (w1:word8,w2) => w1 <= w2
      
    type word = word8

  end (*structure Word8*)


