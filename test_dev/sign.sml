infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

fun print (s:string) : unit = prim("printStringML", s)
fun sign i = if i > 0 then 1 else if i < 0 then ~1 else 0
fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))
fun sameSign (i, j) = sign i = sign j

val maxInt = 2147483647
val minInt = ~2147483648

val () = if sign maxInt > 0 then print "OK\n" else print "ERR\n"

val () = if sign minInt < 0 then print "OK\n" else print "ERR\n"

val () = if sign 1 = sign maxInt then print "OK\n" else print "ERR\n"
val () = if sign ~1 = sign minInt then print "OK\n" else print "ERR\n"

val () = if (sign minInt = ~1 andalso sign maxInt = 1
	     andalso sameSign(minInt, ~1) andalso sameSign(maxInt, 1)) then print "OK\n" else print "ERR\n"
