infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

fun print (s:string) : unit = prim("printStringML", s)
fun printReal (n:real):unit = prim("printReal", n)
fun printNum (i:int) : unit = prim("printNum", i)

fun getCtx () : foreignptr = prim("__get_ctx",())
fun floor (x : real) : int = prim ("floorFloat", (getCtx(),x))    (* may raise Overflow *)
val () = printNum (floor 23.0E23 handle Overflow => 4)

fun real (x:int) : real = prim ("realInt", x)
fun real_to_int (x:real) : int = prim("__real_to_int", x)
fun minIntReal () : real = prim("__minIntReal", ())
fun maxIntReal () : real = prim("__maxIntReal", ())

fun floor2 (r:real) : int =
    if r >= maxIntReal() + 1.0 orelse r < minIntReal() then raise Overflow
    else let val i = real_to_int r
         in if r < real i then i-1 else i
         end

val () = printNum(floor2 2.322 handle Overflow => 4)
val () = printNum(floor2 23.0E23 handle Overflow => 4)

val _ = print "End\n";
