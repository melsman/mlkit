(* `resetRegions` resets all formal region parameters *)

fun size (s : string) : int = prim ("__bytetable_size", s)

fun f () : int =
	let with r
	  val x = "Hi there"`r
	  val _ = resetRegions `[r] ()
	  val y = "Goodbye"`r
	in size y
	end
