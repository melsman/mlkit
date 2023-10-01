(* Exomorphisms by non-unifiable explicit region variables *)

infix ::

fun copy `[r1 r2] (xs : int list`r1) : int list`r2 =
   case xs of
      nil => nil
    | x :: xs => x :: xs
