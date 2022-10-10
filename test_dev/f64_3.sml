infix  7  * / div mod
infix  6  + -
infix  4  = <> > >= < <=

fun printReal (n:real):unit = prim("printReal",n)

fun check (t:real) =
    if t > 0.0 then t else ~2000.0

fun f (a,b,c:real,d,e,f,g,h,i,j) =
    let val t = a * b - c
        val v = check t
    in t + d*e+f*g+i*j + v
    end

val () = printReal (f(5.0,2.0,3.0, 1.0,2.0,3.0,4.0,5.0,6.0,7.0))
