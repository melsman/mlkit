(* `resetRegions` resets all formal region parameters and the regions of `x2`*)

infix +
fun size (s : string) : int = prim ("__bytetable_size", s)

fun f () : int =
	let with r1 r2
	  val x1 = "Hi "`r1
	  val x2 = "there"`r2
	  val _ = forceResetting `[r1] x2
	  val y1 = "Good"`r1
	  val y2 = "bye"`r2
	in size y1 + size y2
	end
