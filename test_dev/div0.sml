infix div
(* fun print (s:string) : unit = prim("printString", "printString", s) *)
fun printNum (i:int) : unit = prim("printNum", "printNum", i)

val a = 5232 div 0

val _ = printNum a
