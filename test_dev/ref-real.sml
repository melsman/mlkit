infix  6  + -
infixr 5  ::
infix  4  = <> > >= < <=
infix  3  := o
type 'a ref = 'a ref

fun !(x: 'a ref): 'a = prim ("!", x)
fun (x: 'a ref) := (y: 'a): unit = prim (":=", (x, y))

fun printReal (n:real):unit = prim("printReal",n)

fun loop 0 f = f ()
  | loop n f = (f (); loop (n-1) f)

val counter = ref 0.0
fun inc () = (counter := (!counter + 1.0); !counter)

val () = (printReal(loop 6000 inc);
	  printReal(loop 1000 inc);
	  printReal(loop 1000 inc))
