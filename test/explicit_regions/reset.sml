(* `resetRegions` resets all region variables in the given value. *)

fun size (s : string) : int = prim ("__bytetable_size", s)

fun f () : int =
	let with r
	  val x = "Hi there"`r
	  val _ = resetRegions x
	  val y = "Goodbye"`r
	in size y
	end
