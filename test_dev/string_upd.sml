infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))

fun not true = false
  | not false = true

fun a <> b = not (a = b)

fun print (s:string) : unit = prim("printStringML", s)

fun sub_unsafe (s:string,i:int) : char = prim ("__bytetable_sub", (s,i))
fun size (s : string) : int = prim ("__bytetable_size", s)
fun (s : string) ^ (s' : string) : string = prim ("concatStringML", (s, s'))
fun alloc_unsafe (i:int) : string = prim("allocStringML", i)
fun update_unsafe (t:string,i:int,c:char) : unit = prim("__bytetable_update", (t, i, c))
fun null() : char = prim("id",0:int)
fun ord (c : char) : int = prim ("id", c)

fun printS s x = print ("String - " ^ x ^ ": '" ^ s ^ "'\n")

val y = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
val () = printS y "0"
val () = update_unsafe(y,5,#"X")
val () = printS y "1"
