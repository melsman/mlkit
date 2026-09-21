infix 6 +
fun plus__noinline(a:int,b:int) = a+b
val w:int = prim("getchar",())
val x = (plus__noinline(9223372036854775807,w) handle Overflow => w+1)
val _:unit = prim("putchar",x)
val _:unit = prim("putchar",10)
