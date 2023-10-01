(* it is an error to unify a local region with a
 * global region variable *)

infix +
fun !(x: 'a ref): 'a = prim ("!", x)

val x = ref 4.5

fun f `[r1] () : real =
  let with r2
      val z = 8.0`r1
      val y = if true then 3.4`r2
              else !x
  in z + y
  end
