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
