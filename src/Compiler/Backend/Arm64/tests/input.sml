(* Runtime input prevents constant folding of the arithmetic in output.sml. *)
val input : word = prim("getchar", ())
