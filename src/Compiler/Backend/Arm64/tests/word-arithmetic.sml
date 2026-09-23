infix 6 + -
infix 7 * mod
infix 4 = <
infixr 5 ::
fun op = (a:word,b:word):bool = prim("=",(a,b))
fun mul__noinline (a:word,b:word) = a*b
fun constant__noinline (a:word) = 0w16807*a
fun mod__noinline (a:word,b:word) = a mod b
fun mod31__noinline (a:word31,b:word31) = a mod b
fun mod32__noinline (a:word32,b:word32) = a mod b
fun eq32 (a:word32,b:word32):bool = prim("=",(a,b))
fun eq31 (a:word31,b:word31):bool = prim("=",(a,b))
val top:word = 0w0-0w1
val caught = (mod__noinline(0w7,0w0); false) handle Div => true
val caught31 = (mod31__noinline(0w7,0w0); false) handle Div => true
val caught32 = (mod32__noinline(0w7,0w0); false) handle Div => true
(* Allocation after exception unwinding must still be able to collect. *)
fun build__noinline (n:int,xs) =
  if n < 1 then xs else build__noinline(n-1,n::xs)
fun length__noinline (nil,n:int) = n
  | length__noinline (_::xs,n) = length__noinline(xs,n+1)
val count = length__noinline(build__noinline(2000,nil),0)
val ok = mul__noinline(top,top) = 0w1 andalso
         mul__noinline(top,0w2) = top-0w1 andalso
         mul__noinline(0w0,top) = 0w0 andalso
         constant__noinline(0w3) = 0w50421 andalso
         constant__noinline(top) = 0w0-0w16807 andalso
         mod__noinline(0w1234567,0w100000) = 0w34567 andalso
         eq31(mod31__noinline(0w1234567,0w100000),0w34567) andalso
         eq32(mod32__noinline(0w1234567,0w100000),0w34567) andalso
         caught andalso caught31 andalso caught32 andalso 1999 < count andalso count < 2001
val _:unit = prim("printStringML",if ok then "OK\n" else "BAD\n")
