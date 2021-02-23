infix  7  * / div mod
infix  6  + - ^
infixr 5  ::
infix  4  = <> > >= < <=

fun numCores () : int =
    prim ("numCores", ())

fun print (s:string) : unit = prim("printStringML", s)
fun printNum (i:int) : unit = prim("printNum", i)

val n = numCores ()

val () = if n >= 1 then print "Ok\n"
         else print "Error\n"

(*val () = printNum n*)
