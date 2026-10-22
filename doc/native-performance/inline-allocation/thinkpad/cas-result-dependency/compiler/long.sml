val _:unit = prim("mlkit_assert_may_spawn",())
infix 6 + -
infix 7 *
infix 4 =
infixr 5 ::
fun op = (a:int,b:int) : bool = prim("=",(a,b))
fun run__noinline (n:int) : int =
  let
    fun build (0,xs) = xs
      | build (n,xs) = build(n-1,1::xs)
    fun sum ([],a) = a
      | sum (x::xs,a) = sum(xs,a+x)
  in sum(build(n,[]),0)
  end
fun repeat (0,a:int) : int = a
  | repeat (n,a) = repeat(n-1,a+run__noinline 100000)
val result : int = repeat(5000,0)
val () = if result = 500000000 then prim("printStringML","ok\n")
         else prim("printStringML","incorrect result\n")
