val _ =
let
  infix  6  + -
  infixr 5  ::
  infix  4  = <> > >= < <=
  infix  3  := o

  fun printReal (n:real):unit = prim("printReal",n)

  val a = 1.0
  val b = 2.0
  val c = a + b

  val _ = printReal a
  val _ = printReal b
  val _ = printReal c

  val d = abs 4.0
  val e = abs ~3.0
  val _ = printReal d
  val _ = printReal e

  val _ = printReal (2.3 - 1.4)

in
  if c > b then printReal 1.0 else printReal ~2.0
end
