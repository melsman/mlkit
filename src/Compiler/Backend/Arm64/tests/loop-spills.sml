infix 6 + -
infix 7 *
infix 4 = <
infix 3 :=
infixr 5 ::
fun op = (a:int,b:int):bool = prim("=",(a,b))
fun ! (r:'a ref):'a = prim("!",r)
fun op := (r:'a ref,v:'a):unit = prim(":=",(r,v))
exception Stop
fun visit__noinline f nil = ()
  | visit__noinline f ((n,used)::rest) =
      (if !used then () else (used := true; f n; used := false);
       visit__noinline f rest)
fun add__noinline total n = total := !total+n
fun repeat__noinline (n:int,f,xs) =
  if n < 1 then () else (visit__noinline f xs; repeat__noinline(n-1,f,xs))
val total = ref 0
val skip = ref true
val use = ref false
val xs = (100,skip)::(7,use)::(100,skip)::(11,use)::nil
val () = repeat__noinline(10000,add__noinline total,xs)
val caught = (visit__noinline (fn _ => raise Stop) xs; false) handle Stop => true
fun product__noinline (a:int,rhs:int) = a*rhs
fun minimum__noinline (n:int) =
  (minimum__noinline(n+n)) handle Overflow => n
val minimum = minimum__noinline (~1)
val overflow = (product__noinline(minimum,~1); false) handle Overflow => true
val arithmetic = product__noinline(7,~3) = ~21 andalso
                 product__noinline(minimum,1) = minimum andalso
                 product__noinline(0,minimum) = 0 andalso overflow
val ok = arithmetic andalso !total = 180000 andalso !skip andalso !use andalso caught
val _:unit = prim("printStringML",if ok then "OK\n" else "BAD\n")
