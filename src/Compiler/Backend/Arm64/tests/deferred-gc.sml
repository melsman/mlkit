infix 6 + -
infix 4 <
infixr 5 ::
fun build__noinline (n:int,xs) =
  if n < 1 then xs else build__noinline(n-1,n::xs)
fun length__noinline (nil,n:int) = n
  | length__noinline (_::xs,n) = length__noinline(xs,n+1)
fun leaf (x:int) = length__noinline(build__noinline(x,nil),0)
val () = _export("arm64_deferred_leaf",leaf)
fun hook (x:int):int = prim("arm64_deferred_inner",x)
val () = _export("arm64_deferred_hook",hook)
val n:int = prim("arm64_deferred_outer",2000)
val m = length__noinline(build__noinline(n,nil),0)
val _:unit = prim("arm64_deferred_verify",())
val _:unit = prim("printStringML",if 1999 < m andalso m < 2001 then "OK\n" else "BAD\n")
