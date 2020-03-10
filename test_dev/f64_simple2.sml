infix  7  * / div mod
infix  6  + -
infix  4  = <> > >= < <=

fun f () = 2.2 + 1.1

fun printReal (n:real):unit = prim("printReal",n)
val () = printReal (f())
