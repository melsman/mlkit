infix  6  + -
infixr 5  ::
infix  4  = <> > >= < <=
infix  3  := o

fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))

fun print (s:string) : unit = prim("printStringML", s)

val x =
 let
   exception E
   exception E' of int
   exception E''
   fun f (E,x)     = print x
     | f (E' 1,x)  = print x
     | f (E'',x)   = print x
     | f (_,x)     = print x
   val _ = f(E,"Test OK\n")
   val _ = f(E' 1,"Test OK\n")
   val _ = f(E'',"Test OK\n")
   val _ = f(E' 2,"Test OK\n")
 in
   2
 end
