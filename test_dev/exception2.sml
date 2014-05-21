infix  6  + -
infixr 5  ::
infix  4  = <> > >= < <=
infix  3  := o
type 'a ref = 'a ref

fun op = (x: ''a, y: ''a): bool = prim ("=", "=", (x, y))

fun print (s:string) : unit = prim("printStringML", "printStringML", s)
fun printNum (n:int):unit = prim("printNum","printNum",n)

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
end;
