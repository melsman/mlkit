
local
  infix  6  + -
  infixr 5  ::
  infix  4  = <> > >= < <=
  infix  3  := o
  type 'a ref = 'a ref

  fun op = (x: ''a, y: ''a): bool = prim ("=", "=", (x, y))

  fun print (s:string) : unit = prim("printStringML", "printStringML", s)
  fun printNum (n:int):unit = prim("printNum","printNum",n)

in (* end of prelude *)
    
  datatype t = A | B of int

  val _ = (raise Bind) handle Bind => (print "Ok - In handle BIND0\n")

  val a = let
	    val (B x) = A
	  in
	    0
	  end handle Bind => (print "Ok - In handle BIND1\n"; 10)
      
  val _ = case a 
	    of 0 => print "Not good - error...\n"
             | 10 => print "Ok, - no error...\n" 
	     | _ => print "Weird - error...\n"

  val _ = (raise Match) handle Match => (print "OK - In handle MATCH0\n")

  val _ = 
    (case a 
       of 0 => print "Not good - error...\n")
    handle Match => print "Ok, - Match no error...\n"

  val _ = print "OK if uncaught exception Match\n"
  val _ = raise Match

end