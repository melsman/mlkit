infix 6 + -
fun step__noinline (w:word) : word = w + 0w1
fun twice__noinline (w:word) : word = step__noinline(step__noinline w)
val input : word = prim("getchar", ())
val _ : unit = prim("putchar", twice__noinline input - 0w1)
val _ : unit = prim("putchar", 0w10)
