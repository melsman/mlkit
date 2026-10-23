infix 6 + -
infix 4 =
fun op = (x: ''a,y: ''a): bool = prim("=",(x,y))
fun loop__noinline(0,acc:int,a:int,b:int,c:int,d:int) = acc+a+b+c+d
  | loop__noinline(n,acc,a,b,c,d) =
      let val r:int = prim("preservation_identity",acc)
      in loop__noinline(n-1,r+n,a,b,c,d) end
val result = loop__noinline(200000000,0,1,2,3,4)
val () = prim("printStringML",if result = 20000000100000010 then "OK\n" else "BAD\n"):unit
