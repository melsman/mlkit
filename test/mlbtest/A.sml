infix :: +

fun print (s:string) : unit = prim("printStringML", s)

(*
fun myhd (x::xs) = x
  | myhd nil = "None"
*)
fun length(x::xs) = 1 + length xs
  | length nil = 0

(*
fun app (f:string->unit) nil = ()
  | app f (x::xs) = (f x; app f xs)
*)
(*
val f : (string -> unit) -> string -> unit = 
    (print "Hello "; fn g => fn s => g s)
*)