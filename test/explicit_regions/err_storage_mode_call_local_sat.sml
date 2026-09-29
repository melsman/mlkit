(* Reject sat call where a local region requires atbot. *)
infix +
fun f `r () : real = 5.4`r
fun g () = let with r in f `sat r () + 1.0 end
