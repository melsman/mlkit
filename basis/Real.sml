
structure Real : REAL =
  struct

    (* Primitives *)

    val radix = 2
    val precision = 53

    fun real (x:int) : real = prim ("realInt", x)

    fun getCtx () : foreignptr = prim("__get_ctx",())

    fun floor (x:real) : int = prim ("floorFloat", (getCtx(),x))    (* may raise Overflow *)
    fun ceil (x:real) : int = prim ("ceilFloat", (getCtx(),x))      (* may raise Overflow *)
    fun trunc (x:real) : int = prim ("truncFloat", (getCtx(),x))    (* may raise Overflow *)

    fun realFloor (x:real) : real = prim ("realFloor", x)
    fun realCeil (x:real) : real = prim ("realCeil", x)
    fun realTrunc (x:real) : real = prim ("realTrunc", x)
    fun realRound (x:real) : real = prim ("realRound", x)

    fun (x:real) / (y:real) : real = prim ("divFloat", (x, y))
    fun rem (x:real, y:real) : real = prim ("remFloat", (x, y))

    fun to_string_gen (s : string) (x : real) : string = prim ("generalStringOfFloat", (s,x))
    fun toString (x:real) : string = prim ("stringOfFloat", x)

    fun sub_unsafe (s:string, i:int) : char = prim ("__bytetable_sub", (s,i))
    fun isNan (x:real) : bool = prim ("isnanFloat", x)

    fun max (x:real, y:real) : real = prim ("__max_real", (x, y))
    fun min (x:real, y:real) : real = prim ("__min_real", (x, y))

    fun copySign (x:real, y:real) : real = prim("copysignFloat", (x, y))
    fun signBit (x:real) : bool = prim("signbitFloat", x)
    fun isNormal (x:real) : bool = prim("isnormalFloat", x)

    fun ldexp (x:real, e:int) : real = prim("ldexpFloat", (x, e))
    fun frexp (x:real) : real * int = prim("frexpFloat", x)

    fun nextAfter (r:real, d:real) : real = prim("nextafterFloat", (r, d))

    fun split (r:real) : {whole:real, frac:real} =
        let val (w,f) = prim("splitFloat", r)
        in {whole=w,frac=f}
        end

    val realMod : real -> real = #frac o split

    type real = real

    structure Math = Math

    val posInf = Initial.posInf
    val negInf = Initial.negInf
    val minPos = Initial.minPos
    val maxFinite = Initial.maxFinite
    val minNormalPos = Initial.minNormalPos

    val fromInt = real

    fun fromLargeInt i =
        let val N_i = 1073741824  (* pow2 30 *)
            val N = IntInf.fromInt N_i
            val N_r = real N_i
            val op < = IntInf.<
            fun fromLargePos i =
                if N < i then
                  let val factor = IntInf.div(i, N)
                      val rem = IntInf.-(i, IntInf.*(factor, N))
                      val factor_r = fromLargePos factor
                      val rem_r = fromLargePos rem
                  in N_r * factor_r + rem_r
                  end
                else real (Int.fromLarge i)
        in if i < 0 then ~ (fromLargePos (IntInf.~ i))
           else fromLargePos i
        end

    (* The following should be replaced by numerically better conversion
     functions; see

     Steele and White : How to print floating-point numbers accurately,
     PLDI'90, pages 112-123, and

     Clinger: How to read floating-point numbers accurately, PLDI'90, pages
     92-101.

     D.M. Gay: Correctly rounded binary-decimal and decimal-binary
     conversions, AT&T Bell Labs, Numerical Analysis Manuscript 90-10,
     November 30, 1990 *)

    fun fmt spec r =
      let fun mlify s = (* Add ".0" if not "e" or "." in s  *)
              let val stop = size s
                  fun loop i =          (* s[0..i-1] contains no "." or "e" *)
                      if i = stop then s ^ ".0"
                      else if sub_unsafe(s,i) = #"." orelse sub_unsafe(s,i) = #"E" then s
                      else loop (i+1)
              in loop 0 end

          open StringCvt
          (* Below we check that the requested number of decimal digits
           * is reasonable; else sml_general_string_of_float may crash. *)
      in
          case spec of
              SCI NONE     => to_string_gen "%e" r
            | SCI (SOME n) =>
                  if n < 0 then raise Size
                  else to_string_gen ("%." ^ Int.toString n ^ "e") r
            | FIX NONE     => to_string_gen "%f" r
            | FIX (SOME n) =>
                  if n < 0 then raise Size
                  else to_string_gen ("%." ^ Int.toString n ^ "f") r
            | GEN NONE     => toString r
            | GEN (SOME n) =>
                  if n < 1 then raise Size
                  else mlify (to_string_gen ("%." ^ Int.toString n ^ "g") r)
            | EXACT => fmt (SCI (SOME 30)) r
      end

    fun getstring str getc source =
        let val len = size str
            fun toLower c = if #"A" <= c andalso c <= #"Z" then Char.chr (Char.ord c + 32)
                            else c
            fun h i src = if i >= len then SOME src
                          else case getc src of
                                   NONE => NONE
                                 | SOME(c, rest) =>
                                   if toLower c = String.sub(str,i) then h (i+1) rest
                                   else NONE
        in h 0 source
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
      in
        case getstring "infinite" getc src1 of
            SOME src' => SOME(if manpos then posInf else negInf,src')
          | NONE =>
        case getstring "inf" getc src1 of
            SOME src' => SOME(if manpos then posInf else negInf,src')
          | NONE =>
        case getstring "nan" getc src1 of
            SOME src' => SOME(posInf - posInf,src')
          | NONE =>
            let val (intg,   src2) = getint src1
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

    fun *+ (a,b,c) = a * b + c
    fun *- (a,b,c) = a * b - c

    fun unordered (x:real, y:real) : bool = isNan x orelse isNan y

    fun compareReal (x:real, y:real) : IEEEReal.real_order =
        let open IEEEReal
        in if unordered(x,y) then UNORDERED
           else if x < y then LESS
           else if y < x then GREATER
           else EQUAL
        end

    fun sign i =
        if isNan i then raise Domain
        else if i > 0.0 then 1
        else if i < 0.0 then ~1
        else 0

    fun compare (x, y: real) =
        if unordered (x,y) then raise IEEEReal.Unordered
        else if x < y then LESS
        else if y < x then GREATER
        else EQUAL

    fun op == (x, y) =
        case compareReal (x,y) of
            IEEEReal.EQUAL => true
          | _ => false

    fun op != (x,y) =
        case compareReal (x,y) of
            IEEEReal.EQUAL => false
          | _ => true

    fun op ?= (a,b) =
        isNan a orelse isNan b orelse op == (a, b)

    infix != ==
    fun isFinite r =
      if isNan r then false
      else r != posInf andalso r != negInf

    fun checkFloat (r:real) =
        if r == posInf orelse r == negInf then raise Overflow
        else if isNan r then raise Div
        else r

    fun sameSign (i, j) = sign i = sign j

    fun class (r:real) : IEEEReal.float_class =
        let open IEEEReal
        in if isNan r then NAN
           else if r == posInf orelse r == negInf then INF
           else if r == 0.0 then ZERO
           else if isNormal r then NORMAL
           else SUBNORMAL
        end

    fun fromManExp {man,exp} : real =
        ldexp(man,exp)

    fun toManExp (r:real) : {man:real, exp:int} =
        let val (m,e) = frexp r
        in {man=m,exp=e}
        end

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

    fun toInt (rm:IEEEReal.rounding_mode) (r:real) : int =
        case rm of
            IEEEReal.TO_NEAREST => round r
          | IEEEReal.TO_NEGINF => floor r
          | IEEEReal.TO_POSINF => ceil r
          | IEEEReal.TO_ZERO => trunc r

    fun toLargeInt rm (r:real) =
        let val N_i = 1073741824  (* pow2 30 *)
            val N = IntInf.fromInt N_i
            val N_r = real N_i
            fun whole r = #whole(split r)
            fun toLargePos r =
                if N_r < r then
                  let val factor_r = whole(r / N_r)
                      val rem_r = r - factor_r * N_r
                      val factor = toLargePos factor_r
                      val rem = toLargePos rem_r
                  in IntInf.+(IntInf.*(N, factor), rem)
                  end
                else Int.toLarge (toInt rm r)
        in if isNan r then raise Domain
           else if r == negInf orelse r == posInf then raise Overflow
           else if r < 0.0 then IntInf.~ (toLargePos (~r))
           else toLargePos r
        end

    fun toLarge r = r
    fun fromLarge _ r = r

    fun toDefault i = i
    fun fromDefault i = i

  end (*structure Real*)

(** SigDoc *)
structure Real64 : REAL = Real

(** SigDoc *)
structure LargeReal : REAL = Real

fun real a = Real.fromInt a
fun floor a = Real.floor a
fun ceil a = Real.ceil a
fun trunc a = Real.trunc a
fun round a = Real.round a
