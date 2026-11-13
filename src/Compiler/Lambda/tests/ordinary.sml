(* Equality-function insertion must traverse record initialisers. *)
fun equal xs = xs = [""]
fun make xs = (xs = [""], equal xs, (length xs, xs))
val (a,b,(n,xs)) = make [""]
val () = if a andalso b andalso n = 1 andalso xs = [""] then ()
         else raise Fail "polymorphic calls in tuple initialisers"
(* Selection/elimination must retain initialiser order and effects. *)
val trace = ref []
fun note n = (trace := !trace @ [n]; n)
val selected = #2 (note 1, note 2, note 3)
val () = if selected = 2 andalso !trace = [1,2,3] then ()
         else raise Fail "tuple initialiser order"
fun select (x,(_,z)) = x + z
val () = if select (1,(2,3)) = 4 then () else raise Fail "argument flattening"
val () = print "ordinary tuple compatibility passed\n"
