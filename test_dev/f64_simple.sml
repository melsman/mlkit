infix  7  * / div mod
infix  6  + -
infix  4  = <> > >= < <=

val a = (1.0,2.0,3.0)
val b = (4.0,5.0,6.0)

fun f () =
    #1(a) * #2(a) + #3(b) * #3(a) + #1(b) - #2(b)
    (* 1 * 2 + 6 * 3 + 4 - 5 = 19 *)

fun printReal (n:real):unit = prim("printReal",n)
val () = printReal (f())
