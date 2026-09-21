(* `resetRegions` on live regions, result in a warning, and no region
   variables are reset. *)

infix +
fun size (s : string) : int = prim ("__bytetable_size", s)

fun f () : int =
	let with r1 r2
	  val x1 = "Hi "`r1
	  val x2 = "there"`r2
	  val _ = resetRegions `[r1] x2
	in size x1 + size x2
	end
