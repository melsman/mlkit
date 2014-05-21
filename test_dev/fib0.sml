infix - +

fun printNum (i:int) : unit = prim("printNum", "printNum", i)

fun fib 0 = 1
  | fib 1 = 1
  | fib n = fib(n-1) + fib(n-2)

val _ = printNum(fib 30)