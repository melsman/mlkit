fun println s = print (s ^ "</br>")

fun tailrec nil acc = acc
  | tailrec (x::xs) acc =
    tailrec xs ((fn() => x)::acc)

fun apply nil acc = acc
  | apply (f::fs) acc = apply fs (f()::acc)

val xs = [132,345,3,2,234,4,5,64,52,34]

fun prl s l = println (s ^ ": [" ^ String.concatWith "," (map Int.toString l) ^ "]")

val () = prl "xs" xs
val fs = tailrec xs nil
val ys = apply fs nil
val () = prl "ys" ys

val () = println(if ys = xs then "OK" else "ERR")
