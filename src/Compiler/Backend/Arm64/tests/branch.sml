infix 4 <
val input : word = prim("getchar", ())
val result = if input < 0w65 then 0w65 else 0w66
val _ : unit = prim("putchar", result)
val _ : unit = prim("putchar", 0w10)
