infix  7  * / div mod
infix  6  + -
infix  4  = <> > >= < <=

fun f (a,b,c:real,d,e,f,g,h,i,j,k,l) = a * b - c + d*e+f*g+i*j+k*l

fun printReal (n:real):unit = prim("printReal",n)

val () = printReal (f(5.0,2.0,3.0, 1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0))
