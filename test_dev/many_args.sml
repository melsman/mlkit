infix  6  +

fun f x = x+x
fun add (x1,x2,x3,x4,x5) = (f x1+x4,f x4+x3,#2(add(f x5+x2,x1,x4,x2,x4)),f x1+x3,f x3+x5+(#3(add(x1,x2,x4,x3,x5))))

val _ = #3(add(1,2,3,4,5))