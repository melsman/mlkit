infix 6 + -
infixr 5 ::
fun eq(a:word,b:word):bool = prim("__equal_word64ub",(a,b))
fun build__noinline(n:word,acc) = if eq(n,0w0) then acc else build__noinline(n-0w1,n::acc)
fun sum__noinline(nil,acc:word) = acc | sum__noinline(x::xs,acc) = sum__noinline(xs,acc+x)
fun depth():word = prim("arm64_region_depth",(prim("__get_ctx",()):word))
val before = depth()
fun !(x:'a ref):'a = prim("!",x)
exception E of word
exception F of word
fun boom__noinline(w:word) =
  let val xs = build__noinline(0w100,nil)
  in raise E (sum__noinline(xs,w)-0w5050)
  end
val w:word = prim("getchar",())
val x = ((boom__noinline w handle E n => raise F (n+0w1)) handle F n => n)
val y = (boom__noinline x handle E n => n)
val _:unit = prim("putchar",if eq(before,depth()) then y else 0w88)
val _:unit = prim("putchar",0w10)
