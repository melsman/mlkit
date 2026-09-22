infix 6 + -
infix 4 = <
infixr 5 ::
exception Fail of string
datatype 'a option = NONE | SOME of 'a
fun op = (a:int,b:int):bool = prim("=",(a,b))
fun ! (r:'a ref):'a = prim("!",r)
fun not true = false | not false = true
fun dec__noinline (n:int) = n-1
fun add__noinline (n:int) = n+2047
fun sub__noinline (n:int) = n-2048
fun negative__noinline (n:int) = n-(~2048)
fun cmp__noinline (n:int) = n < 2047
fun cmpBig__noinline (n:int) = n < 2048
fun cmpNeg__noinline (n:int) = n < ~1
fun nth__noinline (nil,n:int) = raise Fail "empty"
  | nth__noinline (x::xs,n) = if n = 0 then x else nth__noinline(xs,n-1)
fun empty__noinline nil = true | empty__noinline (_::_) = false
fun fields__noinline (t:int*int*int) = #1 t + #2 t + #3 t
fun ref__noinline (r:int ref) = !r
(* Both tagged and untagged integers overflow after enough doublings. *)
fun minimum__noinline (n:int) =
  let val next = SOME(n+n) handle Overflow => NONE
  in case next of NONE => n | SOME m => minimum__noinline m
  end
val minimum = minimum__noinline (~1)
val overflow = (dec__noinline minimum; false) handle Overflow => true
val empty = (nth__noinline(nil,0); false) handle Fail _ => true
val xs = 7::8::9::nil
val ok = dec__noinline 0 = ~1 andalso add__noinline 1 = 2048 andalso
         sub__noinline 1 = ~2047 andalso negative__noinline 1 = 2049 andalso
         cmp__noinline 2046 andalso not(cmp__noinline 2047) andalso
         cmpBig__noinline 2047 andalso not(cmpBig__noinline 2048) andalso
         cmpNeg__noinline (~2) andalso not(cmpNeg__noinline (~1)) andalso
         nth__noinline(xs,2) = 9 andalso empty__noinline nil andalso
         not(empty__noinline xs) andalso fields__noinline(1,2,3) = 6 andalso
         ref__noinline(ref 42) = 42 andalso overflow andalso empty
val _:unit = prim("printStringML",if ok then "OK\n" else "BAD\n")
