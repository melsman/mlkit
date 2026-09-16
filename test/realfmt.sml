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
