
structure Real : REAL =
  struct

    (* Primitives *)

    fun real (x : int) : real = prim ("realInt", x)
    fun floor (x : real) : int = prim ("floorFloat", x)    (* may raise Overflow *)
    fun ceil (x : real) : int = prim ("ceilFloat", x)      (* may raise Overflow *)
    fun trunc (x : real) : int = prim ("truncFloat", x)    (* may raise Overflow *)

    fun realFloor (x: real) : real = prim ("realFloor", x)
    fun realCeil (x: real) : real = prim ("realCeil", x)
    fun realTrunc (x: real) : real = prim ("realTrunc", x)
    fun realRound (x: real) : real = prim ("realRound", x)

    fun (x: real) / (y: real): real = prim ("divFloat", (x, y))
    fun rem (x: real, y: real): real = prim ("remFloat", (x, y))
    fun to_string_gen (s : string) (x : real) : string =
      prim ("generalStringOfFloat", (s,x))
    fun toString (x : real) : string = prim ("stringOfFloat", x)
    fun sub_unsafe (s:string,i:int) : char = prim ("__bytetable_sub", (s,i))
    fun isNan (x : real) : bool = prim ("isnanFloat", x)

    fun max (x: real, y: real) : real = prim ("__max_real", (x, y))
    fun min (x: real, y: real) : real = prim ("__min_real", (x, y))

    type real = real

    structure Math = Math

    val posInf = Initial.posInf
    val negInf = Initial.negInf

    val fromInt = real

    (* The following should be replaced by numerically better conversion
     functions; see

     Steele and White : How to print floating-point numbers accurately,
     PLDI'90, pages 112-123, and

     Clinger: How to read floating-point numbers accurately, PLDI'90, pages
     92-101.

     D.M. Gay: Correctly rounded binary-decimal and decimal-binary
     conversions, AT&T Bell Labs, Numerical Analysis Manuscript 90-10,
     November 30, 1990 *)

    fun fmt spec =
      let fun mlify s = (* Add ".0" if not "e" or "." in s  *)
	      let val stop = size s
		  fun loop i =		(* s[0..i-1] contains no "." or "e" *)
		      if i = stop then s ^ ".0"
		      else if sub_unsafe(s,i) = #"." orelse sub_unsafe(s,i) = #"E" then s
		      else loop (i+1)
	      in loop 0 end

	  open StringCvt
	  (* Below we check that the requested number of decimal digits
	   * is reasonable; else sml_general_string_of_float may crash. *)
	  val fmtspec =
	  case spec of
	      SCI NONE     => to_string_gen "%e"
	    | SCI (SOME n) =>
		  if n < 0 orelse n > 400 then raise Size
		  else to_string_gen ("%." ^ Int.toString n ^ "e")
	    | FIX NONE     => to_string_gen "%f"
	    | FIX (SOME n) =>
		  if n < 0 orelse n > 400 then raise Size
		  else to_string_gen ("%." ^ Int.toString n ^ "f")
	    | GEN NONE     => toString
	    | GEN (SOME n) =>
		  if n < 1 orelse n > 400 then raise Size
		  else (fn r => mlify (to_string_gen ("%." ^ Int.toString n ^ "g") r))
            | EXACT => fmt (SCI (SOME 30))
      in fmtspec
      end

    fun scan getc source =
      let fun decval c = Char.ord c - 48
	  fun pow10 0 = 1.0
	    | pow10 n =
	      if n mod 2 = 0 then
		  let val x = pow10 (n div 2) in x * x end
	      else 10.0 * pow10 (n-1)
	  fun pointsym src =
	      case getc src of
		  NONE           => (false, src)
		| SOME (c, rest) => if c = #"." then (true, rest)
				    else (false, src)
	  fun esym src =
	      case getc src of
		  NONE           => (false, src)
		| SOME (c, rest) =>
		      if c = #"e" orelse c = #"E"  then
			  (true, rest)
		      else (false, src)
	  fun scandigs first next final source =
	      let fun digs state src =
		  case getc src of
		      NONE          => (SOME (final state), src)
		    | SOME(c, rest) =>
			  if Char.isDigit c then
			      digs (next(state, decval c)) rest
			  else
			      (SOME (final state), src)
	      in
		  case getc source of
		      NONE          => (NONE, source)
		    | SOME(c, rest) =>
			  if Char.isDigit c then digs (first (decval c)) rest
			  else (NONE, source)
	      end

	  fun ident x = x
	  val getint  =
	      scandigs real (fn (res, cval) => 10.0 * res + real cval) ident
	  val getfrac =
	      scandigs (fn cval => (1, real cval))
		       (fn ((decs, frac), cval) => (decs+1, 10.0*frac+real cval))
		       (fn (decs, frac) => frac / pow10 decs)
	  val getexp = scandigs ident (fn (res, cval) => 10 * res + cval) ident

	  fun sign src =
	      case getc src of
		  SOME(#"+", rest) => (true,  rest)
		| SOME(#"-", rest) => (false, rest)
		| SOME(#"~", rest) => (false, rest)
		| _                => (true,  src )

	  val src = StringCvt.dropl Char.isSpace getc source
	  val (manpos, src1) = sign src
	  val (intg,   src2) = getint src1
	  val (decpt,  src3) = pointsym src2
	  val (frac,   src4) = getfrac src3

	  fun mkres v rest =
	      SOME(if manpos then v else ~v, rest)

	  fun expopt manval src =
	      let val (esym,   src1) = esym src
		  val (exppos, src2) = sign src1
		  val (expv,   rest) = getexp src2
	      in
		  case (esym, expv) of
		      (_,     NONE)     => mkres manval src
		    | (true,  SOME exp) =>
			  if exppos then mkres (manval * pow10 exp) rest
			  else mkres (manval / pow10 exp) rest
		    | _                 => NONE
	      end
      in
	  case (intg,     decpt, frac) of
	      (NONE,      true,  SOME fval) => expopt fval src4
	    | (SOME ival, false, SOME _   ) => NONE
	    | (SOME ival, true,  NONE     ) => mkres ival src2
	    | (SOME ival, false, NONE     ) => expopt ival src2
	    | (SOME ival, _    , SOME fval) => expopt (ival+fval) src4
	    | _                             => NONE
      end

    fun fromString s = StringCvt.scanString scan s

    val ~       : real -> real        = ~
    val op +    : real * real -> real = op +
    val op -    : real * real -> real = op -
    val op *    : real * real -> real = op *
    val op /    : real * real -> real = op /
    val op >    : real * real -> bool = op >
    val op >=   : real * real -> bool = op >=
    val op <    : real * real -> bool = op <
    val op <=   : real * real -> bool = op <=
    val abs     : real -> real = abs
    fun sign i = if i > 0.0 then 1 else if i < 0.0 then ~1 else 0

    fun compare (x, y: real) =
      if x<y then LESS else if x>y then GREATER else EQUAL

    fun op == (x, y) = case compare (x,y)
		     of EQUAL => true
		      | _ => false
    fun op != (x,y) = case compare (x,y)
			of EQUAL => false
			 | _ => true

    infix != ==
    fun isFinite r =
      if isNan r then false
      else r != posInf andalso r != negInf

    fun sameSign (i, j) = sign i = sign j

    fun round (x : real) : int =
      let (* val _ = print "**R1**\n" *)
	  val t0 = x+0.5
	  (* val _ = print "**R2**\n" *)
	  val floor_t0 = floor t0
	  (* val _ = print "**R3**\n" *)
	  fun even x = x mod 2 = 0
	  (* val _ = print "**R4**\n" *)
      in
	if real(floor_t0) == t0 (* tie *) then
	  let (* val _ = print "**R5**\n" *)
	      val t = floor x
	      (* val _ = print "**R6**\n" *)
	  in if even t then t else floor_t0
	  end
	else floor_t0
      end

    fun toDefault   i   = i
    fun fromDefault i   = i

  end (*structure Real*)

structure Real64 = Real
structure LargeReal = Real

fun real a = Real.fromInt a
fun floor a = Real.floor a
fun ceil a = Real.ceil a
fun trunc a = Real.trunc a
fun round a = Real.round a
