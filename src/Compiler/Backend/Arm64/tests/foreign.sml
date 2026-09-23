infix 6 + -
infix 4 <
fun check b = prim("printStringML",if b then "OK\n" else "BAD\n"):unit
val n:int = prim("@runtime_test1auto",(1,2,3,4,5,6,7,8,9,10))
val () = check(n < 1097 andalso 1095 < n)
val neg:int = prim("@labs",~31)
val () = check(neg < 32 andalso 30 < neg)
infixr 5 ::
fun build__noinline(n:int,xs) = if n < 1 then xs else build__noinline(n-1,n::xs)
fun len__noinline(nil,n:int) = n | len__noinline(_::xs,n) = len__noinline(xs,n+1)
val captured = build__noinline(7,nil)
fun hook (x:int) = x+len__noinline(captured,0)
val () = _export("arm64_hook",hook)
val n:int = prim("arm64_callback",11)
val () = check(n < 19 andalso 17 < n)
val b:bool = prim("@arm64_nonzero",true)
val () = check b
val boxed:int64 = prim("@labs",(~31:int64))
val n:int = prim("@labs",boxed)
val () = check(30 < n andalso n < 32)
val n:int = prim("arm64_raw10",(11,22,33,44,55,66,77,88,99,110))
val () = check(109 < n andalso n < 111)
