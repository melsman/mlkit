infix 6 + -
infix 4 =
fun op = (a:int,b:int) : bool = prim ("=", (a,b))
fun print (s:string) : unit = prim ("printStringML", s)
fun sample () : unit = prim ("mlkit_rp_sample", ())
datatype chain = Nil | Cons of int * chain
fun build__noinline n acc = if n = 0 then acc else build__noinline (n-1) (Cons(n,acc))
fun length__noinline Nil = 0
  | length__noinline (Cons(_,xs)) = 1 + length__noinline xs
val xs = build__noinline 100000 Nil
val _ = sample ()
val _ = if length__noinline xs = 100000 then print "gc allocation ok\n" else print "WRONG\n"
