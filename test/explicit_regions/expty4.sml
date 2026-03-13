(* Type declarations may abstract over region variables, but
   they may be weakened by eluding the region parameters at
   instantiation sites. *)

type t `r1 = string`r1 * string`r1

fun f `[r2 r3] (x:string`r2, y:string`r3) : t = (x, y)

val () = print (op ^ (f ("Hello"," world")) ^ "\n")
