(* Negation and abs act on the sign bit alone: ~0.0 is a negative
 * zero, and abs clears the sign of a NaN.  The values are hidden in a
 * ref so that the compiled code is exercised, not the constant folder. *)

fun p s = print (s ^ "\n")
fun b x = if x then "true" else "false"
val r = ref 0.0
val zero = !r
val nan = zero / zero
val one = 1.0 + zero
val () = p ("signBit (~zero) " ^ b (Real.signBit (~zero))
            ^ ", signBit (~(~zero)) " ^ b (Real.signBit (~(~zero)))
            ^ ", signBit (~one) " ^ b (Real.signBit (~one)))
val () = p ("signBit (abs (~nan)) " ^ b (Real.signBit (Real.abs (~nan)))
            ^ ", signBit (abs (~one)) " ^ b (Real.signBit (Real.abs (~one)))
            ^ ", abs (~2.5) = " ^ Real.toString (Real.abs (~2.5 + zero)))
val () = p ("1/(~zero) = " ^ Real.toString (one / (~zero)))
val () = p ("constants: " ^ b (Real.signBit (~0.0)) ^ " " ^ b (Real.signBit (Real.abs (~1.0))) ^ " " ^ Real.toString (~(~2.5)))
val () = p ("unboxed: " ^ (let val a = Array.fromList [zero, one, ~one] in b (Real.signBit (~(Array.sub (a, 0)))) ^ " " ^ Real.toString (Real.abs (Array.sub (a, 2))) ^ " " ^ Real.toString (~(Array.sub (a, 1))) end))
