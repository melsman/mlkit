exception E of string
datatype tree = Leaf of string | Node of tree list
fun walk (Leaf s) = raise E s
  | walk (Node children) = List.app walk children
val () = walk (Node [Node [Leaf "leaf"]]) handle E s => print (s ^ "\n")
