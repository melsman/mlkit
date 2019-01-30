infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

fun exnName (e: exn) : string = prim("exnNameML", e)   (* exomorphic by copying *)

fun !(x: 'a ref): 'a = prim ("!", x)
fun (x: 'a ref) := (y: 'a): unit = prim (":=", (x, y))
fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))
fun not true = false | not false = true
fun a <> b = not (a = b)
fun print (s:string) : unit = prim("printStringML", s)
fun (s : string) ^ (s' : string) : string = prim ("concatStringML", (s, s'))

val () = print "Hi there\n"

fun build (n,acc) =
    if n = 0 then acc
    else build(n-1,("Hi",n)::acc)

fun len (nil,acc) = acc
  | len (x::xs,acc) = len(xs,acc+1)

fun printNum (n:int):unit = prim("printNum",n)

val r : (string * int) list ref = ref nil

fun loop n = if n < 0 then ()
             else let val x = build (n+1000,nil)
                      val () = r := x
                      val () = printNum(len (x,0))
                  in loop (n-1)
                  end

val () = loop 10
