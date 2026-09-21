(* `resetRegions` on live regions, result in a warning, and no region
   variables are reset. *)

fun size (s : string) : int = prim ("__bytetable_size", s)

fun f () : int =
	let with r
	  val x = "Hi there"`r
	  val _ = resetRegions `[r] ()
	in size x
	end
