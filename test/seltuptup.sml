

fun f () : int * int * int =
    #1((1,2,3),4)
    handle X => raise X

val () = print (if f() = (1,2,3) then "Ok\n" else "Wrong\n")
