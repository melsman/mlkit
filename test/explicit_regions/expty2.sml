(* A locally generated function that is returned can be stored in a passed region *)

infix +
fun printNum (i:int) : unit = prim("printNum", i)

fun g `[r] (a:int) : (int->int)`r =
  fn x => #1(x+2,a)

val () = printNum ((g 6) 8)
