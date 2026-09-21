infixr 5 ::
infix 6 + -
fun eq(a:word,b:word):bool = prim("__equal_word64ub",(a,b))
fun build__noinline(n:word,acc) = if eq(n,0w0) then acc else build__noinline(n-0w1,n::acc)
fun fold__noinline(nil,acc:word) = acc
  | fold__noinline(x::xs,acc) = fold__noinline(xs,acc+x)
fun work__noinline(n:word,w:word) =
  if eq(n,0w0) then w else
  let val xs = build__noinline(0w100,nil)
      val v = fold__noinline(xs,0w0)
  in work__noinline(n-0w1,w+v-0w5050) end
val w:word = prim("getchar",())
val _:unit = prim("putchar",work__noinline(0w1000,w)+0w1)
val _:unit = prim("putchar",0w10)
