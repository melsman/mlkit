
fun runtime_test1 (a1:int,a2:int,a3:int,a4:int,a5:int,
                   a6:int,a7:int,a8:int,a9:int,a10:int) : int =
    prim("runtime_test1", (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10))

fun print (s:string) : unit = prim("printStringML", s)
fun printNum (n:int) : unit = prim("printNum", n)

val x = runtime_test1 (1,2,3,4,5,6,7,8,9,10)

val () = printNum x
