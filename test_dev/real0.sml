val _ =
let
  infix  6  + -
  infixr 5  ::
  infix  4  = <> > >= < <=
  infix  3  := o

  fun printReal (n:real):unit = prim("printReal",n)

  val a = 1.0
  val b = 0.8
  val c = a + b
  val d = a - b
in
(*  val _ = printReal a
  val _ = printReal b*)
  printReal c
; printReal d
end
