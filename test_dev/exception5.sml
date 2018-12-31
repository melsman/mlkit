infix  6  + -
infixr 5  ::
infix  4  = <> > >= < <=
infix  3  := o
type 'a ref = 'a ref

fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))

fun print (s:string) : unit = prim("printStringML", s)
fun printNum (n:int):unit = prim("printNum",n)

fun error b s =
  (if b then
     (print "Ok - ";
      print s)
   else
     (print "Error - ";
      print s);
   print "...\n")

val _ = printNum 1
val _ = print "hej\n"

val _ =
  let
    val _ = print "Testing generative exceptions:\n"
    fun g a =
      let
	fun f x =
	  let
	    exception E
	  in
	    print "Enter f with x=";
	    printNum x;
	    print "\n";
	    if x < 1 then
	      raise E
	    else
	      ((f (x-1)) handle E => (print "In handle E\n"; 7)) (* should not handle this.. *)
	  end
      in
	(f a) handle _ => (print "In handle _\n"; a)
      end (* a *)
  in
    error (g 1 = 1) "exn - generative"
  end
