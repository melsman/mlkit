(*let
  infix  6  + -
  infixr 5  ::
  infix  4  = <> > >= < <=
  infix  3  := o

  fun printReal (n:real):unit = prim("printReal","printReal",n)

val _ = printReal 1.0
*)
(*val a = 1.0 + 3.0*)
(*in 
  ()
end*)

val _ = 1.0