
val a = List.map (fn x => x+1) [1,2,3,4]

val b = foldl (fn (x,acc) => x+acc) 0 [1,2,3,4]

val c = hd [1]

(* should not be inlined, since it has a free occurence of List.Empty *)
val c2 = List.hd [1]

fun mymap f l = List.map f l

val a2 = mymap (fn x => x+1) [1,2]

val x = [1,2] @ [3,4]

val y = rev [1,2]
