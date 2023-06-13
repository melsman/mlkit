(* It is an error to use an effect variable for allocation *)

fun !(x: 'a ref): 'a = prim ("!", x)

val x = ref 4.5

fun f `[e] () : real =
  if true then 3.4`e
  else !x
