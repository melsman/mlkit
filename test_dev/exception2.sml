infix  6  + -
infixr 5  ::
infix  4  = <> > >= < <=
infix  3  := o

fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))

fun print (s:string) : unit = prim("printStringML", s)

val x =
 let
   exception E
   exception A of int
   exception B
   fun f (E,x) = print x
     | f (A 1,x) = print x
     | f (B,x) = print x
     | f (_,x) = print x
   val _ = f(E,"Test OK\n")
   val _ = f(A 1,"Test OK\n")
   val _ = f(B,"Test OK\n")
   val _ = f(A 2,"Test OK\n")
 in
   2
 end
