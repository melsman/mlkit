(* `forceResetting` on live values, result in a warning,
   and forces region variables to be reset. *)

fun size (s : string) : int = prim ("__bytetable_size", s)

fun f () : int =
	let with r
	  val x = "Hi there"`r
	  val _ = forceResetting x
	in size x
	end
