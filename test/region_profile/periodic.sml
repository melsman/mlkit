infix 6 + -
infix 4 =
fun op = (a:int,b:int) : bool = prim ("=", (a,b))
fun limit () : int = prim ("rp_iterations", ())
fun print (s:string) : unit = prim ("printStringML", s)
fun loop (n:int, a:int) = if n = 0 then a else loop(n-1,a+1)
val n = limit()
val result = loop(n,0)
val _ = if result = n then print "periodic ok\n" else print "wrong result\n"
