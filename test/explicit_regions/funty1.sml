(* A local function can be forced to have toplevel effect *)

infix +
fun printNum (i:int) : unit = prim("printNum", i)

val x =
    let val f : int #e0->int = fn x => x+8
    in f 3
    end

val () = printNum x
