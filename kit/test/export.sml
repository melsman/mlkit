val a = ref 34
fun incr (x:int) = x + !a before a:=2

val _ = _export ("incr", incr)

(* Call to do work in C *)
val _ = (prim("work",()):unit)