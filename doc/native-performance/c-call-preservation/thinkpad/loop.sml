infix 6 + -
infix 4 =
fun op = (x: ''a,y: ''a): bool = prim("=",(x,y))
fun loop__noinline(0,acc:int) = acc
  | loop__noinline(n,acc) =
      let val r:int = prim("preservation_identity",acc)
      in loop__noinline(n-1,r+n) end
val result = loop__noinline(200000000,0)
val () = prim("printStringML",if result = 20000000100000000 then "OK\n" else "BAD\n"):unit
