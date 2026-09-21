infix 6 +
exception E of word
fun throw__noinline(w:word) = raise E w
val w:word = prim("getchar",())
val x = (throw__noinline w handle E n => n+0w1)
val _:unit = prim("putchar",x)
val _:unit = prim("putchar",0w10)
