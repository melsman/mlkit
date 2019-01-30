infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

fun print (s:string) : unit = prim("printStringML", s)

val maxInt = 2147483647 : int32
val minInt = ~2147483648 : int32

val () = (maxInt + 1; print "ERR\n") handle Overflow => print "OK (+)\n"
val () = (minInt - 1; print "ERR\n") handle Overflow => print "OK (-)\n"

val () = (maxInt * 2; print "ERR\n") handle Overflow => print "OK (*)\n"
val () = (minInt * 2; print "ERR\n") handle Overflow => print "OK (*)\n"

val () = (~ minInt; print "ERR\n") handle Overflow => print "OK (~)\n"
