infix :: + 

fun print (s:string) : unit = prim("printString", "printString", s)
(*fun myprint (s:string) : unit = prim("printString", "printString", s) *)

fun app f x = f x
