infix 7 *
infix 6 + -
infix 4 = <
infixr 5 ::
exception Fail of string
datatype 'a option = NONE | SOME of 'a
fun op = (a:int,b:int):bool = prim("=",(a,b))
fun ! (r:'a ref):'a = prim("!",r)
fun not true = false | not false = true
fun mul__noinline (x:int,y:int) = x*y
fun neg__noinline (x:int) = ~x
fun abs__noinline (x:int) = abs x
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
fun size__noinline (s:string):int = prim("__bytetable_size",s)
fun byte__noinline (s:string,i:int):int = prim("__bytetable_sub",(s,i))
fun byteKeep__noinline (s:string,i:int) = (byte__noinline(s,i),s,i)
fun truncate__noinline (r:real):int = prim("__real_to_int",r)
fun truncateKeep__noinline (r:real) = (truncate__noinline r,r+1.0)
fun sizeKeep__noinline (s:string) = (size__noinline s,s)
fun ref__noinline (r:int ref) = !r
(* Both tagged and untagged integers overflow after enough doublings. *)
fun minimum__noinline (n:int) =
  let val next = SOME(n+n) handle Overflow => NONE
  in case next of NONE => n | SOME m => minimum__noinline m
  end
val minimum = minimum__noinline (~1)
val overflow = (dec__noinline minimum; false) handle Overflow => true
val mulOverflow = (mul__noinline(minimum,~1); false) handle Overflow => true
val negOverflow = (neg__noinline minimum; false) handle Overflow => true
val absOverflow = (abs__noinline minimum; false) handle Overflow => true
val empty = (nth__noinline(nil,0); false) handle Fail _ => true
val xs = 7::8::9::nil
val (size,s) = sizeKeep__noinline "allocated registers"
val (byte,sameString,sameIndex) = byteKeep__noinline("AZ",1)
val (truncated,nextReal) = truncateKeep__noinline (~3.75)
val ok = mulOverflow andalso negOverflow andalso absOverflow andalso
         mul__noinline(minimum,1) = minimum andalso mul__noinline(~7,6) = ~42 andalso
         neg__noinline 7 = ~7 andalso abs__noinline (~7) = 7 andalso
         abs__noinline 7 = 7 andalso abs__noinline 0 = 0 andalso
         byte = 90 andalso byte__noinline(sameString,sameIndex) = 90 andalso
         truncated = ~3 andalso truncate__noinline nextReal = ~2 andalso
         truncate__noinline 3.75 = 3 andalso truncate__noinline (~0.0) = 0 andalso
         size = 19 andalso size__noinline s = 19 andalso
         size__noinline "" = 0 andalso
         dec__noinline 0 = ~1 andalso add__noinline 1 = 2048 andalso
         sub__noinline 1 = ~2047 andalso negative__noinline 1 = 2049 andalso
         cmp__noinline 2046 andalso not(cmp__noinline 2047) andalso
         cmpBig__noinline 2047 andalso not(cmpBig__noinline 2048) andalso
         cmpNeg__noinline (~2) andalso not(cmpNeg__noinline (~1)) andalso
         nth__noinline(xs,2) = 9 andalso empty__noinline nil andalso
         not(empty__noinline xs) andalso fields__noinline(1,2,3) = 6 andalso
         ref__noinline(ref 42) = 42 andalso overflow andalso empty
val _:unit = prim("printStringML",if ok then "OK\n" else "BAD\n")
