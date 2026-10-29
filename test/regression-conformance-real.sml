local
(* floor, ceil, trunc, round and toInt raise Domain on a NaN and
 * Overflow on an infinity; a NaN passes every range check, so it used
 * to be converted to a meaningless integer. *)
fun p s = print (s ^ "\n")
val r = ref 0.0
val zero = !r
val nan = zero / zero
val inf = 1.0 / zero
fun t name f = p (name ^ ": " ^ (Int.toString (f ()) handle Domain => "Domain" | Overflow => "Overflow"))
fun tl name f = p (name ^ ": " ^ (LargeInt.toString (f ()) handle Domain => "Domain" | Overflow => "Overflow"))
val () = List.app (fn (name, f) => (t (name ^ " nan") (fn () => f nan); t (name ^ " inf") (fn () => f inf); t (name ^ " ~inf") (fn () => f (~inf)); t (name ^ " 2.5") (fn () => f 2.5); t (name ^ " ~2.5") (fn () => f (~2.5))))
                  [("floor", Real.floor), ("ceil", Real.ceil), ("trunc", Real.trunc), ("round", Real.round),
                   ("toInt NEAREST", Real.toInt IEEEReal.TO_NEAREST), ("toInt NEGINF", Real.toInt IEEEReal.TO_NEGINF),
                   ("toInt POSINF", Real.toInt IEEEReal.TO_POSINF), ("toInt ZERO", Real.toInt IEEEReal.TO_ZERO)]
val () = (tl "toLargeInt nan" (fn () => Real.toLargeInt IEEEReal.TO_NEAREST nan); tl "toLargeInt inf" (fn () => Real.toLargeInt IEEEReal.TO_NEAREST inf); tl "toLargeInt 1e20" (fn () => Real.toLargeInt IEEEReal.TO_ZERO 1.0E20))
val () = t "floor 1e300" (fn () => Real.floor 1.0E300)

in
end
local
(* min and max prefer a number to a NaN, nextAfter leaves an infinity
 * and a NaN alone, sameSign compares the sign bits, realRound rounds
 * ties to even like round, and pow(+-1, +-inf) is a NaN. *)
fun p s = print (s ^ "\n")
fun b x = if x then "true" else "false"
val inf = 1.0 / 0.0
val nan = 0.0 / 0.0
val negZero = ~ (0.0 + Real.fromInt 0)
val () = p ("min/max with nan: " ^ Real.toString (Real.min (1.0, nan)) ^ " " ^ Real.toString (Real.min (nan, 1.0)) ^ " " ^ Real.toString (Real.max (1.0, nan)) ^ " " ^ Real.toString (Real.max (nan, 1.0)) ^ " " ^ Real.toString (Real.min (nan, nan)) ^ " " ^ Real.toString (Real.min (1.0, 2.0)) ^ " " ^ Real.toString (Real.max (1.0, 2.0)))
val () = p ("nextAfter: " ^ Real.toString (Real.nextAfter (inf, 1.0)) ^ " " ^ Real.toString (Real.nextAfter (~inf, 1.0)) ^ " " ^ Real.toString (Real.nextAfter (nan, 1.0)) ^ " " ^ Real.toString (Real.nextAfter (1.0, nan)) ^ " " ^ Real.toString (Real.nextAfter (1.0, 1.0)) ^ " " ^ b (Real.nextAfter (1.0, 2.0) > 1.0) ^ " " ^ b (Real.nextAfter (1.0, 0.0) < 1.0) ^ " " ^ Real.toString (Real.nextAfter (Real.maxFinite, inf)))
val () = p ("sameSign: " ^ b (Real.sameSign (0.0, negZero)) ^ " " ^ b (Real.sameSign (~1.0, ~2.0)) ^ " " ^ b (Real.sameSign (~1.0, 2.0)) ^ " " ^ b (Real.sameSign (0.0, 0.0)) ^ " " ^ b (Real.sameSign (negZero, ~3.0)) ^ " " ^ b (Real.signBit negZero) ^ " " ^ b (Real.signBit (Real.copySign (1.0, negZero))))
fun rr x = Real.toString (Real.realRound x) ^ "/" ^ Int.toString (Real.round x)
val () = p ("realRound: " ^ String.concatWith " " (map rr [0.5, 1.5, 2.5, ~0.5, ~1.5, ~2.5, 0.49999, 3.7, ~3.7, 0.0]))
val () = p ("realRound specials: " ^ Real.toString (Real.realRound inf) ^ " " ^ Real.toString (Real.realRound (~inf)) ^ " " ^ Real.toString (Real.realRound nan) ^ " " ^ b (Real.signBit (Real.realRound (~0.25))))
val () = p ("pow: " ^ Real.toString (Math.pow (1.0, inf)) ^ " " ^ Real.toString (Math.pow (~1.0, inf)) ^ " " ^ Real.toString (Math.pow (1.0, ~inf)) ^ " " ^ Real.toString (Math.pow (2.0, inf)) ^ " " ^ Real.toString (Math.pow (2.0, 10.0)) ^ " " ^ Real.toString (Math.pow (1.0, 5.0)) ^ " " ^ Real.toString (Math.pow (nan, 0.0)) ^ " " ^ Real.toString (Math.pow (0.5, inf)))

in
end
local
(* Real.fmt: the exponent is written with as few digits as possible,
 * the special values print the same way in every format, fmt spec
 * raises Size before it sees a value, and toString agrees. *)
fun p s = print (s ^ "\n")
open StringCvt
val inf = 1.0 / 0.0
val nan = 0.0 / 0.0
val () = p (Real.fmt (SCI (SOME 2)) 3.14159 ^ " " ^ Real.fmt (SCI (SOME 0)) 3.14159 ^ " " ^ Real.fmt (SCI (SOME 0)) 314.159 ^ " " ^ Real.fmt (SCI (SOME 3)) ~0.000123 ^ " " ^ Real.fmt (SCI NONE) 1.5 ^ " " ^ Real.fmt (SCI (SOME 1)) 1.5E300 ^ " " ^ Real.fmt (SCI (SOME 1)) 1.5E~300)
val () = p (Real.fmt (FIX (SOME 2)) 3.14159 ^ " " ^ Real.fmt (FIX (SOME 0)) 2.5 ^ " " ^ Real.fmt (FIX NONE) ~1.5 ^ " " ^ Real.fmt (FIX (SOME 3)) 0.0)
val () = p (Real.fmt (GEN (SOME 3)) 3.14159 ^ " " ^ Real.fmt (GEN (SOME 12)) 1.0E~5 ^ " " ^ Real.fmt (GEN (SOME 1)) 100.0 ^ " " ^ Real.fmt (GEN NONE) 123456789012345.0 ^ " " ^ Real.fmt (GEN (SOME 6)) 1.0E10)
val () = p (Real.toString 1.0E300 ^ " " ^ Real.toString 1.0E~5 ^ " " ^ Real.toString 1.0E~10 ^ " " ^ Real.toString 123.0 ^ " " ^ Real.toString ~0.001 ^ " " ^ Real.toString 1.0E15 ^ " " ^ Real.toString 1.0E~4)
fun specials (name, spec) = p (name ^ ": " ^ Real.fmt spec inf ^ " " ^ Real.fmt spec (~inf) ^ " " ^ Real.fmt spec nan)
val () = List.app specials [("FIX", FIX (SOME 2)), ("SCI", SCI (SOME 2)), ("GEN", GEN (SOME 3)), ("GEN NONE", GEN NONE), ("EXACT", EXACT)]
val () = p ("toString: " ^ Real.toString inf ^ " " ^ Real.toString (~inf) ^ " " ^ Real.toString nan)
fun sz f = (f (); "no exception") handle Size => "Size"
val () = p ("fmt (FIX ~1): " ^ sz (fn () => Real.fmt (FIX (SOME ~1))) ^ ", fmt (SCI ~1): " ^ sz (fn () => Real.fmt (SCI (SOME ~1))) ^ ", fmt (GEN 0): " ^ sz (fn () => Real.fmt (GEN (SOME 0))) ^ ", fmt (GEN ~1) 1.0: " ^ sz (fn () => Real.fmt (GEN (SOME ~1)) 1.0) ^ ", fmt (GEN 1): " ^ sz (fn () => Real.fmt (GEN (SOME 1))))

in
end
local
(* Real.toDecimal, fromDecimal, the EXACT format and scan: the decimal
 * digits are the shortest that read back, and reading is correctly
 * rounded. *)
fun p s = print (s ^ "\n")
fun cls IEEEReal.NAN = "NAN" | cls IEEEReal.INF = "INF" | cls IEEEReal.ZERO = "ZERO"
  | cls IEEEReal.NORMAL = "NORMAL" | cls IEEEReal.SUBNORMAL = "SUBNORMAL"
fun dec {class, sign, digits, exp} =
    cls class ^ " " ^ (if sign then "~" else "+") ^ " [" ^ String.concatWith "," (map Int.toString digits) ^ "] " ^ Int.toString exp
fun td x = p ("toDecimal " ^ Real.toString x ^ " = " ^ dec (Real.toDecimal x) ^ " ; EXACT = " ^ Real.fmt StringCvt.EXACT x ^ " ; IEEEReal.toString = " ^ IEEEReal.toString (Real.toDecimal x))
val inf = 1.0 / 0.0
val () = List.app td [1.5, ~12.25, 0.0, ~0.0, 100.0, 0.1, 1.0E~5, 123456789.0, 1.0E300, 5.0E~324, Real.maxFinite, Real.minNormalPos, inf, ~inf, 0.0/0.0, 1.0/3.0, 2.0/3.0]
fun fd (d, s) = p ("fromDecimal " ^ s ^ " = " ^ (case Real.fromDecimal d of NONE => "NONE" | SOME r => Real.fmt StringCvt.EXACT r))
val () = List.app fd [({class = IEEEReal.NORMAL, sign = false, digits = [1,5], exp = 1}, "0.15E1"),
                      ({class = IEEEReal.NORMAL, sign = true, digits = [1,5], exp = 1}, "~0.15E1"),
                      ({class = IEEEReal.NORMAL, sign = false, digits = [], exp = 3}, "no digits"),
                      ({class = IEEEReal.NORMAL, sign = false, digits = [1,10], exp = 1}, "digit 10"),
                      ({class = IEEEReal.NORMAL, sign = false, digits = [~1], exp = 1}, "digit ~1"),
                      ({class = IEEEReal.INF, sign = false, digits = [], exp = 0}, "inf"),
                      ({class = IEEEReal.INF, sign = true, digits = [], exp = 0}, "~inf"),
                      ({class = IEEEReal.ZERO, sign = true, digits = [], exp = 0}, "~zero"),
                      ({class = IEEEReal.NAN, sign = false, digits = [], exp = 0}, "nan"),
                      ({class = IEEEReal.NORMAL, sign = false, digits = [1], exp = ~5}, "0.1E~5"),
                      ({class = IEEEReal.NORMAL, sign = false, digits = [4,9,4,0,6,5,6,4,5,8,4,1,2,4,6,5,4], exp = ~323}, "minPos")]
fun rt x = Real.== (x, valOf (Real.fromString (Real.fmt StringCvt.EXACT x)))
           andalso Real.== (x, valOf (Real.fromDecimal (Real.toDecimal x)))
           andalso Real.== (x, #1 (valOf (Real.scan Substring.getc (Substring.full (Real.fmt StringCvt.EXACT x)))))
val () = p ("round trips: " ^ (if List.all rt [8.767554402351378E~06, 7.389793634414673E~04, 1.147142857142857E03, 8.742857142857143E01, 0.1, 1.0/3.0, 1.0E300, 5.0E~324, Real.maxFinite, 123456.789, ~9.87654321E~200] then "ok" else "bad"))
fun sc s = p ("scan \"" ^ s ^ "\" = " ^ (case Real.scan Substring.getc (Substring.full s) of NONE => "NONE" | SOME (r, rest) => Real.fmt StringCvt.EXACT r ^ " rest \"" ^ Substring.string rest ^ "\""))
val () = List.app sc ["+1.5", "-1.5", "~1.5", "1.", ".5", "1.5E3", "1.5e3", "1.5e~3", "1.5e-3", "1.5e+3", "42", "1.5e", "1.5abc", "1.5.5", ".", "abc", "", "  7.25x", "inf", "-infinity", "nan", "0.1000000000000000055511151231257827021181583404541015625", "1e400", "1e-400", "123456789012345678901234567890"]

in
end

local
  (* Continuations forwarding a parsed exponent can spill its sign flag.
   * A one-word local frame must survive a tail call with stack arguments. *)
  fun exponent (text,expected) =
      case IEEEReal.fromString text of
          SOME {exp,...} => if exp = expected then () else raise Fail "decimal exponent sign"
        | NONE => raise Fail "decimal exponent scan"
  val () = List.app exponent [("1.0e~1",0),("1.0e-1",0),("1.0e~1000",~999)]
in
end
