infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

local
fun exnName (e: exn) : string = prim("exnNameML", e)   (* exomorphic by copying *)

fun !(x: 'a ref): 'a = prim ("!", x)
fun (x: 'a ref) := (y: 'a): unit = prim (":=", (x, y))
fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))
fun not true = false | not false = true
fun a <> b = not (a = b)
fun print (s:string) : unit = prim("printStringML", s)
fun (s : string) ^ (s' : string) : string = prim ("concatStringML", (s, s'))
fun printNum (i:int) : unit = prim("printNum", i)

in
fun fib x = if x < 2 then 1 else fib(x-1)+fib(x-2)

type thread = foreignptr

val rf = ref(fn () => fib 42)
val f = !rf
val fp_f : foreignptr = prim("pointer", f) (* very unsafe *)
val () = prim("function_test", fp_f)

val t : thread = prim("spawnone", fp_f)

fun get (t:thread) : int = prim("thread_get", t)

val () = print "hi there\n"
val () = printNum (get t)
val () = print "hi again\n"

end
