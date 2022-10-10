infix  7  * / div mod
infix  6  + -
infix  4  = <> > >= < <=

fun f (a,b,c:real) = a * b - c

fun printReal (n:real):unit = prim("printReal",n)

val () = printReal (f(5.0,2.0,3.0))  (* 7.00 *)
