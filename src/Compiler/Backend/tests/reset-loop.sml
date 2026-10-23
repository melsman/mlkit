infix 6 + -
infix 4 =
fun op = (x: ''a,y: ''a):bool = prim("=",(x,y))
fun !(x:'a ref):'a = prim("!",x)
val result =
  let with r
    fun loop__noinline(0,acc:int) = acc
      | loop__noinline(n,acc) =
          let val x = ref`r n
              val value = !x
              val _ = forceResetting `[r] ()
          in loop__noinline(n-1,acc+value) end
  in loop__noinline(200000000,0) end
val () = prim("printStringML",if result=20000000100000000 then "reset loop passed\n" else "BAD\n"):unit
