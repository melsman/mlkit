



  infix  6  + -
  infixr 5  ::
  infix  4  = <> > >= < <=
  infix  3  := o
  type 'a ref = 'a ref

  fun !(x: 'a ref): 'a = prim ("!", "!", x) 
  fun (x: 'a ref) := (y: 'a): unit = prim (":=", ":=", (x, y)) 

  fun printReal (n:real):unit = prim("printReal","printReal",n)

(*val _ = 
  let
    val _ = printReal(1.0)
    val a = 1.0
    val _ = printReal a
    val b = 3.0
    val _ = printReal b
    val c = a + b
    val _ = printReal(c)
  in
    ()
  end


local
  val counter = ref 0.0
in
  fun inc() = (counter := !counter + 1.0; !counter)
  fun succ() = (counter := !counter - 1.0; !counter)
end
*)
fun loop 0 f = f ()
  | loop n f = (f ();
		loop (n-1) f)

(*val res = (loop 10 (fn () => printReal(inc()));
	   loop 10 (fn () => printReal(succ())))
*)
val counter = ref 0.0
fun inc () = (counter := (!counter + 1.0);
	      printReal(!counter);
	      !counter)

val res = (loop 6000 inc;
	   loop 1000 inc;
	   loop 1000 inc)
