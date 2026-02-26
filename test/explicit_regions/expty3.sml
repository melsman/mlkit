(* Type declarations may abstract over region variables *)

type `r1 t = string`r1 * string`r1

fun f `[r r2] (x:string`r2) : `r t = (x ^ "", x ^ "")

val () = print (op ^ (f "Hello") ^ "\n")
