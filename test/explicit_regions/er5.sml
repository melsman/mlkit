(* it is an error to declare a region variable that is already in
 * scope; we may later allow for shadowing *)

fun f `[r] () : real =
  let region r2 r
  in #1 (4.3`r, 4.8`r2)
  end
