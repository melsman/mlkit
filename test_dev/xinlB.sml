
fun printNum (i:int) : unit = prim("printNum", i)

val () = printNum(incr())
val () = printNum(incr())

fun iota n = if n <= 0 then [] else iota(n-1) @ [n]
fun repl a n = if n <= 0 then [] else a :: repl a (n-1)

val ys = scan (op +) 0 (iota 10)

val _ = map printNum ys
