(* It is an error to refer to a region that is not in
 * scope (function parameter) *)

infix +
fun f `[r] () : real = 4.3`r
fun g () = let with r2
           in 0.0 + f `[r3] ()
           end
