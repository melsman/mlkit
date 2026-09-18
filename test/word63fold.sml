(* Shifts and conversions of 63-bit words and integers with constant
 * operands are folded at compile time; the folding used to be done at
 * 64 bits, so that a signed shift did not replicate the top bit and
 * Int.fromLarge accepted values outside the 63-bit range. *)

fun p s = print (s ^ "\n")
fun w s w = p (s ^ " = 0wx" ^ Word63.toString w)
fun i s f = p (s ^ " = " ^ (Int63.toString (f ()) handle Overflow => "Overflow"))

val () = w "~>> top 62" (Word63.~>> (0wx4000000000000000, 0w62))
val () = w "~>> top 1" (Word63.~>> (0wx4000000000000000, 0w1))
val () = w "~>> top 0" (Word63.~>> (0wx4000000000000000, 0w0))
val () = w "~>> 1 62" (Word63.~>> (0w1, 0w62))
val () = w "~>> allOnes 5" (Word63.~>> (0wx7FFFFFFFFFFFFFFF, 0w5))
val () = w ">> top 62" (Word63.>> (0wx4000000000000000, 0w62))
val () = w "<< 1 62" (Word63.<< (0w1, 0w62))
val () = w "<< 1 63" (Word63.<< (0w1, 0w63))
val () = w "Word63 ~>> top 62" (Word63.fromLarge (Word63.toLarge (Word63.~>> (Word63.fromLarge 0wx4000000000000000, 0w62))))
val () = i "Int.fromLarge 2^62" (fn () => Int63.fromLarge (IntInf.pow (IntInf.fromInt 2, 62)))
val () = i "Int.fromLarge (2^62 - 1)" (fn () => Int63.fromLarge (IntInf.- (IntInf.pow (IntInf.fromInt 2, 62), IntInf.fromInt 1)))
val () = i "Int.fromLarge ~2^62" (fn () => Int63.fromLarge (IntInf.~ (IntInf.pow (IntInf.fromInt 2, 62))))
val () = i "Int.fromLarge (~2^62 - 1)" (fn () => Int63.fromLarge (IntInf.- (IntInf.~ (IntInf.pow (IntInf.fromInt 2, 62)), IntInf.fromInt 1)))
val () = i "Int63.fromLarge 2^62" (fn () => Int63.fromLarge (IntInf.pow (IntInf.fromInt 2, 62)))
