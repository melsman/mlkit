
val a = map (fn x => x+1) [1,2,3,4]

val b = List.foldl (fn (x,acc) => x+acc) 0 [1,2,3,4]

