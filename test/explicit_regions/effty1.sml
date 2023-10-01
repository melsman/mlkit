(* A function with a local effect cannot escape *)

fun printNum (i:int) : unit = prim("printNum", i)

val f =
    let with e
        val g : int #e-> int = fn x => x
    in g
    end
val () = printNum (f 3)
