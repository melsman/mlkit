

fun ceil (x : real) : int = prim ("ceilFloat", "ceilFloat", x)       (* may raise Overflow *)

val _ = ceil 1E32 handle Overflow => 43

val _ = raise Match
