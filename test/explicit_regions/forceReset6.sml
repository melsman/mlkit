(* `forceResetting` on live values, result in a warning,
   and forces region variables to be reset. *)

infix +
fun size (s : string) : int = prim ("__bytetable_size", s)

fun f () : int =
	let with r1 r2
	  val x1 = "Hi "`r1
	  val x2 = "there"`r2
	  val _ = forceResetting `[r1] x2
	in size x1 + size x2
	end
