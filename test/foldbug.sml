datatype 'a tree = L | T of 'a tree * 'a * 'a tree

fun fold (_, L, acc) = acc
  | fold (f, T (l, x, r), acc) = fold (f, l, (f (x, fold (f, r, acc))))

val v = fold (fn (x, a) => x+a, T(L,40,T(L,2,L)), 0)

val () = print (Int.toString v ^ "\n")
