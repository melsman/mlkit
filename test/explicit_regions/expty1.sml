(* A local function can be forced into a global region *)

infix +
fun printNum (i:int) : unit = prim("printNum", i)

val x : int =
    let region r
        val t = 3.0 + 4.0
        val f : (int->int)`r0top = fn x => #1(x+2,t)
    in f 4
    end

val () = printNum x
