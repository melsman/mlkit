(* Inconsistent use of a parameter region. A region cannot hold both
 * pairs and strings. *)

fun f `[r] (a:string`r) : (int * string)`r = (3, a)
