(* it is an error to unify a local region with a
 * region parameter *)

infix +
fun f `[r1] () : real =
  let region r2
      val z = 8.0`r1
      val y = if true then 3.4`r2
              else z
  in 2.0 + y
  end
