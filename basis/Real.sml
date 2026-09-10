
structure Real : REAL =
  struct

    (* Primitives *)

    val radix = 2
    val precision = 53

    fun real (x:int) : real = prim ("realInt", x)

    fun real_to_int (x:real) : int = prim("__real_to_int", x)
    fun minIntReal () : real = prim("__minIntReal", ())
    fun maxIntReal () : real = prim("__maxIntReal", ())

    fun getCtx () : foreignptr = prim("__get_ctx",())

    fun isNan (x:real) : bool = prim ("isnanFloat", x)

    (* "These raise Overflow if the resulting value cannot be represented
       as an int, and Domain if r is NaN" -- a NaN passes every range
       check, so it must be taken out first. *)
    fun floor (r:real) : int =
        if isNan r then raise Domain
        else if r >= maxIntReal() + 1.0 orelse r < minIntReal() then raise Overflow
        else let val i = real_to_int r
             in if r < real i then i-1 else i
             end

(*    fun floor (x:real) : int = prim ("floorFloat", (getCtx(),x))    (* may raise Overflow *) *)
    fun ceil_ (x:real) : int = prim ("ceilFloat", (getCtx(),x))      (* may raise Overflow *)
    fun trunc_ (x:real) : int = prim ("truncFloat", (getCtx(),x))    (* may raise Overflow *)
    fun ceil x = if isNan x then raise Domain else ceil_ x
    fun trunc x = if isNan x then raise Domain else trunc_ x

    fun realFloor (x:real) : real = prim ("realFloor", x)
    fun realCeil (x:real) : real = prim ("realCeil", x)
    fun realTrunc (x:real) : real = prim ("realTrunc", x)
    fun realRound (x:real) : real = prim ("realRound", x)

    fun (x:real) / (y:real) : real = prim ("divFloat", (x, y))
    fun rem (x:real, y:real) : real = prim ("remFloat", (x, y))

    fun to_string_gen (s : string) (x : real) : string = prim ("generalStringOfFloat", (s,x))
    fun toString (x:real) : string = prim ("stringOfFloat", x)

    fun sub_unsafe (s:string, i:int) : char = prim ("__bytetable_sub", (s,i))

    fun max_ (x:real, y:real) : real = prim ("__max_real", (x, y))
    fun min_ (x:real, y:real) : real = prim ("__min_real", (x, y))

    (* "If one argument is NaN, the other is returned." *)
    fun max (x, y) = if isNan x then y else if isNan y then x else max_ (x, y)
    fun min (x, y) = if isNan x then y else if isNan y then x else min_ (x, y)

    (* The correctly rounded value of a decimal numeral in C syntax. *)
    fun strtod_ (s : string) : real = prim ("strtodFloat", s)

    fun copySign (x:real, y:real) : real = prim("copysignFloat", (x, y))
    fun signBit (x:real) : bool = prim("signbitFloat", x)
    fun isNormal (x:real) : bool = prim("isnormalFloat", x)

    fun ldexp (x:real, e:int) : real = prim("ldexpFloat", (x, e))
    fun frexp (x:real) : real * int = prim("frexpFloat", x)

    fun nextAfter_ (r:real, d:real) : real = prim("nextafterFloat", (r, d))

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

    (* scan reads the numeral --  [+~-]?([0-9]+(.[0-9]+)? | .[0-9]+)([eE][+~-]?[0-9]+)?
       and the spellings of the infinities and NaN -- and hands the text
       to strtod, which rounds correctly; accumulating the digits in
       floating point does not, and EXACT output did not read back. *)
    fun scan getc source =
      let fun isDigit c = #"0" <= c andalso c <= #"9"
          fun digits src acc =
              case getc src of
                  SOME (c, rest) => if isDigit c then digits rest (acc ^ String.str c)
                                    else (acc, src)
                | NONE => (acc, src)
          fun sign src =
              case getc src of
                  SOME(#"+", rest) => (false, rest)
                | SOME(#"-", rest) => (true, rest)
                | SOME(#"~", rest) => (true, rest)
                | _                => (false, src)
          val src = StringCvt.dropl Char.isSpace getc source
          val (neg, src1) = sign src
          fun signed v = if neg then ~v else v
      in
        case getstring "infinity" getc src1 of
            SOME src' => SOME(signed posInf, src')
          | NONE =>
        case getstring "inf" getc src1 of
            SOME src' => SOME(signed posInf, src')
          | NONE =>
        case getstring "nan" getc src1 of
            SOME src' => SOME(signed (posInf - posInf), src')
          | NONE =>
            let val (ip, src2) = digits src1 ""
                (* a point belongs to the numeral only when digits follow
                   it: "12." is 12 followed by "." *)
                val (fp, src3) = case getc src2 of
                                     SOME(#".", rest) =>
                                       (case digits rest "" of
                                            ("", _) => (NONE, src2)
                                          | (fp, src3) => (SOME fp, src3))
                                   | _ => (NONE, src2)
            in
              if ip = "" andalso fp = NONE then NONE
              else
                let val (expo, src4) =
                        case getc src3 of
                            SOME(c, rest) =>
                              if c = #"e" orelse c = #"E" then
                                let val (eneg, rest1) = sign rest
                                    val (ed, rest2) = digits rest1 ""
                                in if ed = "" then ("", src3)
                                   else ("e" ^ (if eneg then "-" else "") ^ ed, rest2)
                                end
                              else ("", src3)
                          | NONE => ("", src3)
                    val text = (if neg then "-" else "")
                               ^ (if ip = "" then "0" else ip) ^ "."
                               ^ (case fp of SOME fp => fp | NONE => "")
                               ^ expo
                in SOME (strtod_ text, src4)
                end
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

    fun op == (x:real, y) =
        x >= y andalso y >= x

    fun op != (x,y) =
        not (op == (x,y))

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

    (* sameSign is equality of the sign bits, so that 0.0 and ~0.0 differ. *)
    fun sameSign (i, j) = signBit i = signBit j

    fun class (r:real) : IEEEReal.float_class =
        let open IEEEReal
        in if isNan r then NAN
           else if r == posInf orelse r == negInf then INF
           else if r == 0.0 then ZERO
           else if isNormal r then NORMAL
           else SUBNORMAL
        end

    (* "If either argument is NaN, this returns NaN.  If r is +-infinity, it
       returns +-infinity." -- C's nextafter steps off an infinity. *)
    fun nextAfter (r, d) =
        if isNan r then r
        else if isNan d then d
        else if r == posInf orelse r == negInf then r
        else nextAfter_ (r, d)

    local
      fun cstring s = String.translate (fn #"~" => "-" | c => String.str c) s
      fun rev' ([], acc) = acc
        | rev' (x :: xs, acc) = rev' (xs, x :: acc)
    in
      (* toDecimal produces the shortest sequence of decimal digits that
         reads back as the same real: the %e format is tried with more
         and more digits until strtod agrees, and 17 significant digits
         always suffice for a double. *)
      fun toDecimal r =
          let open IEEEReal
              val a = abs r
              fun shortest p =
                  let val s = to_string_gen ("%." ^ Int.toString p ^ "e") a
                  in if Int.>= (p, 16) orelse strtod_ (cstring s) == a then s
                     else shortest (Int.+ (p, 1))
                  end
              (* s is d.dddE<exp>: the digits go before the point, so
                 0.ddddE<exp+1> is the same number *)
              fun split s =
                  let val n = size s
                      fun getc i = if Int.< (i, n) then SOME (String.sub (s, i), Int.+ (i, 1)) else NONE
                      fun mant i ds =
                          if Int.>= (i, n) then (ds, n)
                          else let val c = String.sub (s, i)
                               in if c = #"E" then (ds, Int.+ (i, 1))
                                  else if c = #"." then mant (Int.+ (i, 1)) ds
                                  else mant (Int.+ (i, 1)) (Int.- (Char.ord c, 48) :: ds)
                               end
                      val (rds, ei) = mant 0 []
                      val e = case Int.scan StringCvt.DEC getc ei of
                                  SOME (e, _) => e
                                | NONE => 0
                      fun stripZeros (0 :: ds) = stripZeros ds
                        | stripZeros ds = ds
                  in (rev' (stripZeros rds, []), Int.+ (e, 1))
                  end
          in case class r of
                 (* a NaN has no sign to report: fmt writes every NaN as "nan" *)
                 NAN => {class = NAN, sign = false, digits = [], exp = 0}
               | INF => {class = INF, sign = signBit r, digits = [], exp = 0}
               | ZERO => {class = ZERO, sign = signBit r, digits = [], exp = 0}
               | cls => let val (digits, exp) = split (shortest 0)
                        in {class = cls, sign = signBit r, digits = digits, exp = exp}
                        end
          end

      fun fromDecimal {class, sign, digits, exp} =
          let open IEEEReal
              fun signed r = if sign then ~r else r
              fun valid [] = true
                | valid (d :: ds) = Int.<= (0, d) andalso Int.<= (d, 9) andalso valid ds
              fun str [] = ""
                | str (d :: ds) = String.str (Char.chr (Int.+ (d, 48))) ^ str ds
          in case class of
                 NAN => SOME (signed (posInf - posInf))
               | INF => SOME (signed posInf)
               | ZERO => SOME (signed 0.0)
               | _ => if valid digits then
                        SOME (signed (strtod_ ("0." ^ str digits ^ "e" ^ cstring (Int.toString exp))))
                      else NONE
          end
    end

    fun fmt spec =
      let fun mlify s = (* Add ".0" if not "e" or "." in s  *)
              let val stop = size s
                  fun loop i =          (* s[0..i-1] contains no "." or "e" *)
                      if i = stop then s ^ ".0"
                      else if sub_unsafe(s,i) = #"." orelse sub_unsafe(s,i) = #"E" then s
                      else loop (Int.+ (i, 1))
              in loop 0 end

          open StringCvt
          (* The exception is raised when fmt spec is evaluated, before any
           * value is seen.  Below we also check that the requested number
           * of decimal digits is reasonable; else
           * sml_general_string_of_float may crash. *)
          val () = case spec of
                       SCI (SOME n) => if Int.< (n, 0) then raise Size else ()
                     | FIX (SOME n) => if Int.< (n, 0) then raise Size else ()
                     | GEN (SOME n) => if Int.< (n, 1) then raise Size else ()
                     | _ => ()
      in
          fn r =>
          case spec of
              SCI NONE     => to_string_gen "%e" r
            | SCI (SOME n) => to_string_gen ("%." ^ Int.toString n ^ "e") r
            | FIX NONE     => to_string_gen "%f" r
            | FIX (SOME n) => to_string_gen ("%." ^ Int.toString n ^ "f") r
            | GEN NONE     => toString r
            | GEN (SOME n) =>
                  if isFinite r then mlify (to_string_gen ("%." ^ Int.toString n ^ "g") r)
                  else toString r
            | EXACT => IEEEReal.toString (toDecimal r)
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
