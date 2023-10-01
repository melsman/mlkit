(* Different threads should not store values in the same region. Here
   specified with a disjointness constraint on the put effects of the
   two functions (par specified with 'fun'). *)

fun par `[e1 e2] (f: unit #e1 -> 'a) (g: unit #e2 -> 'b) : ('a * 'b) while e1 ## e2 =
  (f(),g())

val () =
    let val (a,b) = par (fn () => (1,2)) (fn () => (3,4))
        val c = if true then a else b
    in ()
    end
