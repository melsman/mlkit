infix 6 + -
infix 7 *
val result : word = (input + 0w3) * 0w2 - 0w69
val _ : unit = prim("putchar", result)
val _ : unit = prim("putchar", 0w10)
