(* Check that even when the optimiser is disabled (-no_opt), calls to
argument-transformed functions are still transformed... *)

val s = "Hello" ^ " world\n"

val () = print s
