(* it is an error to unify two region parameters *)

infix +
fun f `[r1 r2] () : real =
  let val z = 8.0`r1
      val y = if true then 3.4`r2
              else z
  in 2.0 + y
  end
