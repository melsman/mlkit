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

val s = "aHello there - this is a quite long string, but not too longa"

val sz = size s

val () = print "First: "
val () = if ord(sub_unsafe(s,0)) = 97 then print "Ok\n"
         else print "Err\n"

val () = print "Seccond: "
val () = if ord(sub_unsafe(s,1)) = 72 then print "Ok\n"
         else print "Err\n"

val () = print "Second to last: "
val () = if ord(sub_unsafe(s,sz-2)) = 103 then print "Ok\n"
         else print "Err\n"

val () = print "Last: "
val () = if ord(sub_unsafe(s,sz-1)) = 97 then print "Ok\n"
         else print "Err\n"

val () = print "==Now updating==\n"

fun printS s x = print ("String - " ^ x ^ ": '" ^ s ^ "'\n")

val y = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
val () = printS y "0"
val () = update_unsafe(y,5,#"X")
val () = printS y "1"

val () = print "Now s\n"
val () = printS s "0"
val () = update_unsafe(s,sz-2,#"d")
val () = printS s "1"
val () = update_unsafe(s,sz-1,#"e")
val () = printS s "2"
val () = update_unsafe(s,0,#"b")
val () = printS s "3"
val () = update_unsafe(s,1,#"c")
val () = printS s "4"

val () = print "First: "
val () = if sub_unsafe(s,0) = #"b" then print "Ok\n"
         else print "Err\n"

val () = print "Second: "
val () = if sub_unsafe(s,1) = #"c" then print "Ok\n"
         else print "Err\n"

val () = print "Second to last: "
val () = if sub_unsafe(s,sz-2) = #"d" then print "Ok\n"
         else print "Err\n"

val () = print "Last: "
val () = if sub_unsafe(s,sz-1) = #"e" then print "Ok\n"
         else print "Err\n"
