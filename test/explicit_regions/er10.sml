(* it is an error for a value in an explicit local
 * region to escape *)

infix +
fun f () : real =
  let region r
      val y = 3.4`r
  in if true then 2.0 else y
  end
