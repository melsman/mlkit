infix 6 +
fun apply__noinline (f,x:word) = f x
fun make__noinline (n:word) = fn (x:word) => n+x
val w:word = prim("getchar",())
val f = make__noinline w
val _:unit = prim("putchar", apply__noinline(f,0w1))
val _:unit = prim("putchar",0w10)
