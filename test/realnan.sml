(* Comparisons with a NaN are false.  The values are hidden in a ref so
 * that both the compiled code and the constant folder are exercised. *)

fun p s = print (s ^ "\n")
fun b x = if x then "true" else "false"
val r = ref 0.0
val zero = !r
val nan = zero / zero
val one = 1.0 + zero
val () = p ("nan < 1 " ^ b (nan < one) ^ ", nan <= 1 " ^ b (nan <= one)
            ^ ", nan > 1 " ^ b (nan > one) ^ ", nan >= 1 " ^ b (nan >= one))
val () = p ("1 < nan " ^ b (one < nan) ^ ", 1 <= nan " ^ b (one <= nan)
            ^ ", 1 > nan " ^ b (one > nan) ^ ", 1 >= nan " ^ b (one >= nan))
val () = p ("if nan < 1: " ^ (if nan < one then "yes" else "no")
            ^ ", if nan <= 1: " ^ (if nan <= one then "yes" else "no")
            ^ ", if nan > 1: " ^ (if nan > one then "yes" else "no")
            ^ ", if nan >= 1: " ^ (if nan >= one then "yes" else "no"))
val () = p ("1 < 2 " ^ b (one < 2.0) ^ ", 2 < 1 " ^ b (2.0 < one)
            ^ ", 1 <= 1 " ^ b (one <= 1.0) ^ ", 1 > 2 " ^ b (one > 2.0)
            ^ ", 1 >= 1 " ^ b (one >= 1.0) ^ ", 2 >= 1 " ^ b (2.0 >= one))
val () = p ("constants: " ^ b (0.0/0.0 < 1.0) ^ " " ^ b (1.0 < 2.0) ^ " " ^ b (2.0 <= 1.0))
val () = p ("Real.== (nan, nan) " ^ b (Real.== (nan, nan)) ^ ", Real.?= (nan, 1) " ^ b (Real.?= (nan, one)))
