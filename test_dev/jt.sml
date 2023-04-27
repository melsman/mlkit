infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

fun print (s:string) : unit = prim("printStringML", s)
fun printNum (n:int):unit = prim("printNum", n)

fun f x =
    case x of
        0 => 2
      | 1 => 5
      | 2 => 8
      | 3 => 1
      | 4 => 4
      | 5 => 0
      | 6 => 7
      | 7 => 9
      | 8 => 3
      | 9 => 6
      | _ => 10

(*fun f x = x+2*)

fun map f [] = []
  | map f (x::xs) = f x :: map f xs

fun app f [] = ()
  | app f (x::xs) = (f x ; app f xs)

val () = app printNum (map f [1,2])
