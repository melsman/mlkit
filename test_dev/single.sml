
fun printNum (n:int):unit = prim("printNum", n)

fun print (s:string) : unit = prim("printStringML", s)

infix + :: = <> ^

fun (s : string) ^ (s' : string) : string = prim ("concatStringML", (s, s'))

fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))
fun not true = false | not false = true
fun a <> b = not (a = b)

datatype t = A of int

val f =
 fn (A 0) => "zero"
  | (A 1) => "one"
  | (A 2) => "two"
  | (A 3) => "three"
  | (A 100) => "one hundred"
  | (A 1000) => "one thousand"
  | (A 22) => "twenty two"
  | (A 50) => "fifty"
  | (A 30) => "thirty"
  | _ => "other"

fun sum nil (acc:int) = acc
  | sum (A x::xs) acc = sum xs (acc+x)

fun app f nil = ()
  | app f (x::xs) = (f x ; app f xs)

val a = [A 100, A 30, A 1000, A 1, A 0, A 50, A 72, A 2]

val () = app (fn x => print (f x ^ "\n")) a

val () = if A 34 <> A 1000 andalso A 100 = A 100
         then print "Eq OK\n"
         else print "Eq WRONG\n"

val () = printNum (sum a 0) (* 1255 *)
