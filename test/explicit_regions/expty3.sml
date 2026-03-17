(* Type declarations may abstract over region variables *)

type t `r1 = string`r1 * string`r1

fun f `[r r2] (x:string`r2) : t `r = (x ^ "", x ^ "")

val () = print (op ^ (f "Hello") ^ "\n")
