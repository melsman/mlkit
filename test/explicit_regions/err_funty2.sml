(* Inconsistent use of a parameter region. A region cannot hold both
 * pairs and triples. *)

fun f `[r] () : (((int * int)`r -> (int * int)`r) * int * bool)`r =
    (fn z => z, 3, false)
