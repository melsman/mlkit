(* implode, explode virker ikke i copy string *)
(*kitlife35u.sml*)

(*based on kitlifeopt.sml, but with copying
  to avoid many generations in the same region*)

local

infix  6  +
infixr 5  ::

fun printNum(i:int):unit = prim ("printNum","printNum",i)

fun print (x:string):unit = prim("printString","printString",x)
fun append [] ys = ys
  | append (x::xs) ys = x :: append xs ys
  
fun map f [] = []
  | map f (a::x) = f a :: map f x

fun length l = 
   case l of
    [] => 0 
  | x::xs => length xs

val bail = [(0,0),(0,1)]
val _ = print "Before genB\n"
val genB = length(append bail (map (fn x => x) bail))
val _ = print "After genB\n"

in

end

