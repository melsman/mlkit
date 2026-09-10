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
