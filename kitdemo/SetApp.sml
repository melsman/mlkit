structure IntSet = Set(type elem = int
                       val lt = op <
                       fun pr a = Int.toString a)
open IntSet
val _ = print (pr (union(singleton 2, singleton 5)))
