local
infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

fun printReal (n:real):unit = prim("printReal",n)
fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))
fun print (s:string) : unit = prim("printStringML", s)

datatype e = R of real
           | Add of e * e
           | Mul of e * e
           | Sum of e list
           | Var of string
           | Let of string * e * e

fun eval (f:string -> real) (e:e) : real =
    case e of
        R r => r
      | Add (e1,e2) => eval f e1 + eval f e2
      | Mul (e1,e2) => eval f e1 * eval f e2
      | Var x => f x
      | Sum nil => 0.0
      | Sum (x::xs) => eval f (Add(x,Sum xs))
      | Let (x,e1,e2) =>
        let val v = eval f e1
        in eval (fn y => if x=y then v else f y) e2
        end

val a = Let("a",Add(R 5.0, Mul(R 3.0, R 2.0)),Add(Sum[Var "a",R 2.0,R 3.2], Mul(Var "a", R 2.0)))

val v = eval (fn _ => 0.0) a

in
val () = printReal v
end
