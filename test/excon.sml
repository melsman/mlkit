datatype t = Opt

exception Opt2

val rec Opt = fn (a : int) => a + 6

fun Opt2 0 = 0 
  | Opt2 n = n + Opt2(n-1)

val a = Opt2 10 + Opt 10

val _ = print (Int.toString a ^ "\n")