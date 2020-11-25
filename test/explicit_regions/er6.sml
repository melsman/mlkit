(* it is an error to unify a region parameter with a
 * global region variable *)

fun !(x: 'a ref): 'a = prim ("!", x)

val x = ref 4.5

fun f `[r] () : real =
  if true then 3.4`r
  else !x
