infix 4 =
fun op = (x:''a, y:''a) : bool = prim ("=", (x,y))
val _ = (0.0:real16) = 0.0
