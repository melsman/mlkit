(* Reject atbot call where a formal region requires sat. *)
fun f `r () : real = 5.4`r
fun g `r () : real = f `atbot r ()
