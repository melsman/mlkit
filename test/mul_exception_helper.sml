exception E of int
fun fail k = E k
fun app f [] = ()
  | app f (x::xs) = (f x; app f xs)
fun f (n:int) : unit = if n = 0 then raise fail 1 else app f [n-1]
val () = f 5 handle E k => print (Int.toString k ^ "\n")
