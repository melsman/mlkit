
fun runtime_test0 (a1:int,a2:int,a3:int) : int =
    prim("@runtime_test0", (a1,a2,a3))

fun print (s:string) : unit = prim("printStringML", s)
fun printNum (n:int) : unit = prim("printNum", n)

val x = runtime_test0 (1,2,3)

val () = printNum x
