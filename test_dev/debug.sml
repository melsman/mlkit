val _ = print "\nFile general.sml: Testing structure General...\n"

exception NoExceptionRaised

fun getExn (f : unit -> 'a) = 
    (f (); NoExceptionRaised) handle e => (e)

fun prExn(exnStr, exn) =
    (print "\nShould be `"; print exnStr; print "':\n  ";
     print (exnName exn); print "\n  ";
     print (exnMessage exn));

exception E1;
exception E2 = E1;
val _ = prExn("Subscript", getExn(fn _ => Vector.sub(vector[], ~1)))
