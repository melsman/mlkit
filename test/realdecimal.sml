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
